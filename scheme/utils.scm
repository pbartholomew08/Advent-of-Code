;;; Advent of Code Scheme utilities.
;;; Copyright (C) 2021  Paul Bartholomew
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Load required modules
(use-modules (ice-9 rdelim))

(define (read-file f)
  ;; Reads a file line-by-line, returining a list with each line as an element.
  ;;
  ;; @param f : String of filename to be read.
  ;; @returns : List whose elements represent a line in the file.
  (with-input-from-file f
    (lambda ()
      (define (file-to-list lst ln)
	;; Recursively read each line in a file, appending to a list until the EOF character is
	;; reached, returning the list of lines.
	;;
	;; @param lst : List of lines read so far.
	;; @param ln  : Most recently read line.
	;; @returns   : List of lines.
	(if (eof-object? ln)
	    lst
	    (file-to-list (append lst (list ln))
			  (read-line))))
      (file-to-list '() (read-line)))))

(define (read-numbers-from-file f)
  ;; Reads a file, returning a list with each string converted to a number.
  ;;
  ;; @param f : String of filename to be read.
  ;; @returns : List of numbers whose elements represent a line in the file.
  (map string->number (read-file f)))
