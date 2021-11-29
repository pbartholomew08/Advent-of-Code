;;; Advent of Code 2020/day1 Scheme implementation.
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

;;; Day 1.
;;; Given a list of numbers, find the pair whose sum is 2020 and return the product.
;;;
;;; As an example, given '(1721 979 366 299 675 1456) the pair '(1727 299) sum to 2020 and the
;;; answer 1721*299=514579.
;;;
;;; For given input, solution is 898299.

(use-modules (srfi srfi-1))
(load "../utils.scm")

(define (find-combination lst prop)
  ;; Given a list, find a combination of entries that satisfy the property procedure.
  ;;
  ;; @param lst  : List of input values.
  ;; @param prop : Procedure to be satisfied by a valid combination.
  ;; @returns    : List of the entries that satisfy the property or
  ;;               #f is no valid combination is found.
  (define (run-search v next s)
    ;; Helper procedure to find the combination in a list that satisfies the property.
    ;; Given a potential value and a list to search, tests each value in turn; if this list is
    ;; exhausted without success, start again with a new candidate value until all possible
    ;; combinations have been tried.
    ;;
    ;; @param v    : Candidate value for a valid combination.
    ;; @param next : A list of values that have been tested already, to be used in subsequent
    ;;               searches if current one fails.
    ;; @param s    : List of potential values to search and test in combination with v.
    ;; @returns    : List of the entries tat satisfy the property or
    ;;               #f if no valid combination is found.
    (if (null? s)
	(if (null? next)
	    #f ; Failed to find a pair.
	    (run-search (car next) '() (cdr next)))
	(if (prop (list v (car s)))
	    (list v (car s))
	    (run-search v
			(append next (list (car s)))
			(cdr s)))))
  (run-search (car lst) '() (cdr lst)))

(define (day1 lst tgt)
  ;; Procedure for the 2020 day1 puzzle.
  ;;
  ;; @param lst : List of values containing a pair whose sum is some specific value.
  ;; @param tgt : The target value of the pair sum.
  ;; @returns   : The product of the pair whose sum = tgt.
  (fold * 1 (find-combination lst
			      (lambda (lst)
				(= (fold + 0 lst)
				   tgt)))))

(define (test-day1)
  ;; Test procedure for 2020 day1 based on the given example.
  ;;
  ;; @returns : #t or #f indicating passing or failing of the test.
  (let ((input '(1721 979 366 299 675 1456))
	(expect 514579))
    (= (day1 input 2020) expect)))

(define (run-day1 datfile tgt)
  (day1 (read-numbers-from-file datfile)
	tgt))
