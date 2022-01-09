;;; -*- Mode: Scheme; Character-encoding: utf-8; -*-
;;; Copyright (C) 2005-2020 beingmeta, inc.  All rights reserved.
;;; Copyright (C) 2020-2022 Kenneth Haase (ken.haase@alum.mit.edu).

(in-module 'text/readcsv)

(module-export! '{read-csv read-csv-file})

(use-module 'texttools)

(define (convert-cell val)
  (if (string? val)
      (if (and (has-prefix val "\"")  (has-suffix val "\""))
	  (convert-cell (subseq val 1 -1))
	  (if (compound-string? val) val
	      (if (empty-string? val) val
		  (string->lisp val))))
      val))

(define (read-csv string (cols #f) (cellsep ","))
  (let* ((rows (segment string "\n"))
	 (cols (or cols (map convert-cell (segment (first rows) cellsep)))))
    (for-choices (row (elts rows 1))
      (tryif (not (or (empty-string? row)
		      (has-prefix row "#")
		      (has-prefix row ";")))
	(let ((row (map convert-cell (segment row cellsep))))
	  (let ((f (frame-create #f)))
	    (dotimes (i (length row))
	      (store! f (elt cols i) (elt row i)))
	    f))))))

(define (read-csv-file file (cols #f) (cellsep ","))
  (read-csv (filestring file) cols cellsep))
