;;; Advent of Code 2021/day2 Scheme implementation.
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

;;; 2021/day2
;;;
;;; Given a series of commands of the form
;;;
;;;   forward X
;;;   down Y
;;;   up Z
;;;
;;; the submarine can be made to move forward X units, increase its depth Y units or decrease its
;;; depth Z units.
;;; As an example:
;;;
;;;   forward 5
;;;   down 5
;;;   forward 8
;;;   up 3
;;;   down 8
;;;   forward 2
;;;
;;; results in a final position (starting from 0) of 15 horizontal, 10 deep which multiplied
;;; together gives 150.

(use-modules (srfi srfi-1))

(define example-input
  '(("forward" 5)
    ("down" 5)
    ("forward" 8)
    ("up" 3)
    ("down" 8)
    ("forward" 2)))
(define expected-horizontal 15)
(define expected-depth 10)
(define expected-solution 150)

(define (make-sub h d a)
  (list h d a))
(define (sub-horizontal sub)
  (car sub))
(define (sub-depth sub)
  (cadr sub))
(define (sub-aim sub)
  (caddr sub))

(define (move-sub sub h d)
  (make-sub (+ (sub-horizontal sub) h)
	    (+ (sub-depth sub) d)))

(define (instruction-direction instruction)
  (car instruction))
(define (instruction-distance instruction)
  (cadr instruction))
(define (decode-instruction instruction)
  (cond ((string=? (instruction-direction instruction) "forward")
	 (lambda (s)
	   (move-sub s (instruction-distance instruction) 0)))
	((string=? (instruction-direction instruction) "down")
	 (lambda (s)
	   (move-sub s 0 (instruction-distance instruction))))
	((string=? (instruction-direction instruction) "up")
	 (decode-instruction (list "down" (- (instruction-distance instruction)))))
	(else
	 #f)))

(define (command-sub sub instruction)
  ((decode-instruction instruction) sub))

(define (pilot-sub sub instruction-list)
  (if (null? instruction-list)
      sub
      (pilot-sub (command-sub sub (car instruction-list))
		 (cdr instruction-list))))

(define (test-day2-horizontal)
  (= (sub-horizontal (pilot-sub (make-sub 0 0) example-input))
     expected-horizontal))
(define (test-day2-depth)
  (= (sub-depth (pilot-sub (make-sub 0 0) example-input))
     expected-depth))
(define (test-day2-sub-product)
  (= (fold * 1 (pilot-sub (make-sub 0 0) example-input))
     expected-solution))

(define (run-day2 f)
  (fold *
	1
	(pilot-sub (make-sub 0 0)
		   (map (lambda (s)
			  (define (convert-last-to-number l)
			    (list (car l)
				  (string->number (cadr l))))
			  (convert-last-to-number (string-split s #\space)))
			(read-file f)))))
