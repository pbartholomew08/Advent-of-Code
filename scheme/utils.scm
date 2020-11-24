;;;; -*- mode: Scheme -*-
;;
;;; utils.scm - Advent of Code utilities for scheme
;;

(use-modules (ice-9 rdelim))

(define-module (utils)
  #:export (parse-file-to-list
	    read-file-to-list))

;;; Parse a file, returning a list of parsed lines
;;
;;; Inputs:
;;  - file: the file to parse
;;  - p: parsing function, this is applied to each line before adding the list
;;
;;; Returns:
;;  - row-by-row list of file's contents after parsing.
(define (parse-file-to-list file p)
  (with-input-from-file file
    (lambda ()
      (let f ((line (read-line)))
	(if (eof-object? line)
	    line
	    (cons line (f (read-line))))))))

;;; Read a file, returning a list of lines
;;
;;; Inputs:
;;  - file: the file to read
;;
;;; Returns:
;;  - row-by-row list of file's contents.
(define (read-file-to-list file)
  (parse-file-to-list file identity))
