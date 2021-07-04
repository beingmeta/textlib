;;; -*- Mode: Scheme; Character-encoding: utf-8; -*-
;;; Copyright (C) 2005-2020 beingmeta, inc.  All rights reserved.
;;; Copyright (C) 2020-2021 beingmeta, llc.

(in-module 'text/tables)

(module-export! '{text/table/open text/table/readrow text/table/read
		  table/open table/read
		  text/table/open/csv
		  text/table/open/tabbed
		  text/table/open/piped})

(module-export! '{text/table/symbol text/table/number text/table/arg})

(use-module '{texttools varconfig text/parsetime ezrecords reflection})

(define-init default-cell-separator "\t")
(varconfig! text:table:cellsep default-cell-separator)
(varconfig! text:table:separator default-cell-separator)

(define-init default-cellfn #default)
(varconfig! text:table:cellfn default-cellfn)

(define-init default-empty-cell #default)
(varconfig! text:table:emptycell default-cell-empty)

(define-init default-table-comment #f)
(varconfig! text:table:comment default-table-comment)

;;;; The table structure

(define (text/table->string table)
  (stringout "#<TABLE " (write (text/table-id table)) ">"))

(defrecord (text/table MUTABLE OPAQUE `(stringfn . text/table->string))
  id
  port
  cellsep
  cellfn
  emptyval
  readrow
  comment
  opts)

(define (text/table/open source (opts-or-sep #f) (opts-or-sep-or-fn #f))
  "Returns a `text/table` object reading from *source*, which can be an ioport, a filename, "
  "a multi-line string, or a **thunk** which returns a string or object (which will be "
  "returned directly by `text/table/readrow`.) Options include:\n"
  "*  `cellsep` separates cells within a row (string); it can be a string, a regex, or "
  "a **textmatch** pattern;\n"
  "*  `cellfn` is called on each cell value to generate a value;\n"
  "*  `emptyval` is a value used to indicate empty cells;\n"
  "*  `comment` indicates a prefix (string, regex, or **textmatch** pattern "
  "which indicates lines/rows to be ignored;\n"
  (local opts
    (if (opts? opts-or-sep) opts-or-sep
	(if (opts? opts-or-sep-or-fn) opts-or-sep-or-fn
	    #f)))
  (local cellsep
    (if (not (opts? opts-or-sep)) opts-or-sep
	(or opts-or-sep-or-fn
	    (getopt opts 'cellsep (getopt opts 'separator default-cell-separator)))))
  (local cellfn
	 (if (and (not (opts? opts-or-sep)) (not (opts? opts-or-sep-or-fn)))
	     opts-or-sep-or-fn
	     (getopt opts 'cellfn default-cellfn)))
  (when (character? cellsep) (set! cellsep (string cellsep)))
  (local port (cond ((thunk? source) source)
		    ((port? source) source)
		    ((not (string? source)) (irritant source |InvalidTableSource|))
		    ((multiline-string? source) (open-input-string source))
		    ((file-exists? source) (open-input-file source (getopt opts 'openopts)))
		    (else (irritant source  |InvalidTableSource|))))
  (cons-text/table (if (applicable? port)
		       (or (procedure-name port) (stringout port))
		       (if (port? port) (portid port)
			   (irritant port '|InvalidTableSource|)))
		   port
		   cellsep
		   cellfn
		   (getopt opts 'emptycell default-empty-cell)
		   (getopt opts 'readrow
			   (getopt opts 'rowsep
				   (and (testopt opts 'rowmatch)
					(getopt opts 'rowmatch))))
		   (getopt opts 'comment #f default-table-comment)
		   opts))

(define (read-row port readrow comment-rule)
  (if comment-rule
      (let ((row "") (done #f))
	(until done
	  (set! row
	    (cond ((applicable? port) (port))
		  ((not readrow) (getline port))
		  ((applicable? readrow) (readrow port))
		  ((string? readrow) (read-record port readrow))
		  (else (text/read-match port readrow))))
	  (if (or (fail? row)
		  (not (string? row))
		  (not (string-starts-with? row comment-rule)))
	      (set! done #t)))
	row)
      (cond ((applicable? port) (port))
	    ((not readrow) (getline port))
	    ((applicable? readrow) (readrow port))
	    ((string? readrow) (read-record port readrow))
	    (else (text/read-match port readrow)))))


(define (text/table/readrow table)
  "Reads a row (typically a vector) from *table*"
  (let* ((port (text/table-port table))
	 (cellsep (text/table-cellsep table))
	 (cellfn (text/table-cellfn table))
	 (emptyval (text/table-emptyval table))
	 (readrow (text/table-readrow table))
	 (comment-rule (text/table-comment table))
	 (check-empty (not (default? emptyval)))
	 (opts (text/table-opts table)))
    (let ((row (read-row port readrow comment-rule)))
      (if (string? row)
	  (forseq (cell (text/slice row cellsep #f))
	    (cond ((and check-empty (or (fail? cell) (not cell) (empty-string? cell)))
		   emptyval)
		  ((not (string? cell)) cell)
		  ((default? cellfn) (text/table/cleancell cell))
		  (cellfn (cellfn cell))
		  (else cell)))
	  row))))

(define (text/table/cleancell string)
  (if (multiline-string? string)
      (trim-spaces string)
      (stdspace string)))

;;;; Aliases

(define text/table/read (fcn/alias text/table/readrow))
(define table/read (fcn/alias text/table/readrow))
(define table/open (fcn/alias text/table/open))

(define (text/table/open/csv source (opts #f))
  (text/table/open source ","  opts))
(define (text/table/open/tabbed source (opts #f))
  (text/table/open source "\t" opts))
(define (text/table/open/piped source (opts #f))
  (text/table/open source "|" opts))

;;;; Cellfns

(define (text/table/symbol string)
  (cond ((empty-string? string) #f)
	((multiline-string? string) string)
	(else (string->symbol (downcase (textsubst string '(spaces) "_"))))))
(define (text/table/number string)
  (or (string->number string) string))
(define (text/table/arg string)
  (cond ((empty-string? string) #f)
	((multiline-string? string) string)
	(else (parse-arg string))))

