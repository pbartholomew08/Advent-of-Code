;;; Advent of Code 2021/day3 Scheme implementation.
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

;;; 2021/day3
;;;
;;; The sub's diagnostic information is returned as a binary number, encoding GAMMA and EPSILON as
;;; the most and least common bits at each position, e.g. given
;;;
;;;   00100 11110 10110 10111 10101 01111 00111 11100 10000 11001 00010 01010
;;;
;;; then GAMMA=10110=22 and EPSILON=01001=9.
;;; The power consumption is given by their product.

(use-modules (srfi srfi-1))

(define example-input '(#b00100
			#b11110
			#b10110
			#b10111
			#b10101
			#b01111
			#b00111
			#b11100
			#b10000
			#b11001
			#b00010
			#b01010))

(define (count-even lst)
  (fold + 0 (map (lambda (x)
		   (if (even? x)
		       1
		       0))
		 lst)))
(define (majority-even? lst)
  (>= (count-even lst)
      (/ (length lst) 2)))

(define (divide-list-by lst n)
  (map (lambda (x)
	 (floor (/ x n)))
       lst))
(define (compute-gamma lst)
  (define (run-calculation g ctr n lst)
    (if (= ctr n)
	g
	(run-calculation (if (majority-even? lst)
			     g
			     (+ g (expt 2 ctr)))
			 (+ ctr 1)
			 n
			 (divide-list-by lst 2))))
  (run-calculation 0 0 (string-length (number->string (list-max lst) 2)) lst))

(define (compute-epsilon lst)
  (define (init-epsilon e ctr n)
    (if (= ctr n)
	e
	(init-epsilon (+ e (expt 2 ctr))
		      (+ ctr 1)
		      n)))
  (- (init-epsilon 0 0 (string-length (number->string (list-max lst) 2)))
     (compute-gamma lst)))

(define (run-day3 lst)
  (* (compute-gamma lst)
     (compute-epsilon lst)))

(define (list-max lst)
  (define (find-max m lst)
    (if (null? lst)
	m
	(find-max (max m (car lst))
		  (cdr lst))))
  (find-max (car lst) (cdr lst)))
