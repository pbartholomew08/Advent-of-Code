;;; Advent of Code 2021/day1 Scheme implementation.
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

;;; 2021/day1
;;;
;;; Given a series of numbers representing the depth of the ocean floor at a point, how many times
;;; does it increase wrt the previous measurement?
;;; As an example, given
;;;
;;;   '(199 200 208 210 200 207 240 269 260 263)
;;;
;;; the depth increases 7 times (there is no increase for the first value as there is no previous
;;; measurement).

(define example-input '(199 200 208 210 200 207 240 269 260 263))
(define (list-delta lst)
  ;; Given a list compute the difference between current and previous values.
  ;; As first value has no previous, make its "delta" 0.
  (define (compute-delta prev lst deltas)
    (if (null? lst)
	deltas
	(compute-delta (car lst)
		       (cdr lst)
		       (append deltas (list (- (car lst)
					       prev))))))
  (compute-delta (car lst) lst '()))

(define (sliding-sum lst window-width)
  (define (run current-window in out)
    (cond ((null? in)
	   (append out (list (fold + 0 current-window))))
	  ((< (length current-window) window-width)
	   (run (append current-window (list (car in)))
		(cdr in)
		out))
	  (else
	   (run (append (cdr current-window) (list (car in)))
		(cdr in)
		(append out (list (fold + 0 current-window)))))))
  (run '() lst '()))

(define (test-sliding-sum)
  (equal? (sliding-sum example-input 1)
	  example-input))

(define (count-positives lst)
  ;; Given a list of numbers count the number of positive (>0) values.
  (fold +
	0
	(map (lambda (x)
	       (if (> x 0)
		   1
		   0))
	     lst)))

(define (day1 lst window-width)
  (sum-positives (list-delta (sliding-sum lst window-width))))

(define (test-day1-1)
  ;; Test day 1 solution against example input for part 1.
  (= (day1 example-input
	   1)
     7))
(define (test-day1-2)
  ;; Test day 1 solution against example input for part 2.
  (= (day1 example-input
	   3)
     5))
