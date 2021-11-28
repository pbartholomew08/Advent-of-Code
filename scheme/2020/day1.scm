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

(define (find-combination lst prop)
  (define (run-search v next s)
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
  (fold * 1 (find-combination lst
			      (lambda (lst)
				(= (fold + 0 lst)
				   tgt)))))

(define (test-day1)
  (define input '(1721 979 366 299 675 1456))
  (define expect 514579)
  (= (day1 input 2020) expect))
