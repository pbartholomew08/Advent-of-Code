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

(use-modules (srfi srfi-1)
	     (aoc utils))

(define example-input
  '(("forward" 5)
    ("down" 5)
    ("forward" 8)
    ("up" 3)
    ("down" 8)
    ("forward" 2)))
(define expected-horizontal-1 15)
(define expected-depth-1 10)
(define expected-solution-1 150)

(define expected-horizontal-2 15)
(define expected-depth-2 60)
(define expected-solution-2 900)

(define (make-sub h d a)
  (list h d a))
(define (sub-horizontal sub)
  (car sub))
(define (sub-depth sub)
  (cadr sub))
(define (sub-aim sub)
  (caddr sub))
(define (sub-hd s)
  (* (sub-horizontal s)
     (sub-depth s)))

(define (move-sub sub h d a)
  (make-sub (+ (sub-horizontal sub) h)
	    (+ (sub-depth sub) d)
	    (+ (sub-aim sub) a)))

(define (instruction-direction instruction)
  (car instruction))
(define (instruction-distance instruction)
  (cadr instruction))
(define (decode-instruction instruction piloting-mode-h piloting-mode-d)
  (cond ((string=? (instruction-direction instruction) "forward")
	 (lambda (s)
	   (let ((h (instruction-distance instruction)))
	     (move-sub s h (piloting-mode-h s h) 0))))
	((string=? (instruction-direction instruction) "down")
	 (lambda (s)
	   (let ((d (instruction-distance instruction)))
	     (move-sub s 0 (piloting-mode-d s d) d))))
	((string=? (instruction-direction instruction) "up")
	 (decode-instruction (list "down" (- (instruction-distance instruction)))
			     piloting-mode-h
			     piloting-mode-d))
	(else
	 #f)))

(define (command-sub sub instruction piloting-mode-h piloting-mode-d)
  ((decode-instruction instruction piloting-mode-h piloting-mode-d)
   sub))

(define (pilot-sub sub piloting-mode-h piloting-mode-d instruction-list)
  (if (null? instruction-list)
      sub
      (pilot-sub (command-sub sub (car instruction-list) piloting-mode-h piloting-mode-d)
		 piloting-mode-h
		 piloting-mode-d
		 (cdr instruction-list))))

(define (piloting-mode-h-1 s h)
  0)
(define (piloting-mode-d-1 s d)
  d)

(define (piloting-mode-h-2 s h)
  (* h (sub-aim s)))
(define (piloting-mode-d-2 s d)
  0)

(define (test-day2-horizontal-1)
  (= (sub-horizontal (pilot-sub (make-sub 0 0 0)
				piloting-mode-h-1
				piloting-mode-d-1
				example-input))
     expected-horizontal-1))
(define (test-day2-depth-1)
  (= (sub-depth (pilot-sub (make-sub 0 0 0)
			   piloting-mode-h-1
			   piloting-mode-d-1
			   example-input))
     expected-depth-1))
(define (test-day2-sub-product-1)
  (= (sub-hd (pilot-sub (make-sub 0 0 0)
			piloting-mode-h-1
			piloting-mode-d-1
			example-input))
     expected-solution-1))
(define (test-day2-1)
  (and (test-day2-horizontal-1)
       (test-day2-depth-1)
       (test-day2-sub-product-1)))

(define (test-day2-horizontal-2)
  (= (sub-horizontal (pilot-sub (make-sub 0 0 0)
				piloting-mode-h-2
				piloting-mode-d-2
				example-input))
     expected-horizontal-2))
(define (test-day2-depth-2)
  (= (sub-depth (pilot-sub (make-sub 0 0 0)
			   piloting-mode-h-2
			   piloting-mode-d-2
			   example-input))
     expected-depth-2))
(define (test-day2-sub-product-2)
  (= (sub-hd (pilot-sub (make-sub 0 0 0)
			piloting-mode-h-2
			piloting-mode-d-2
			example-input))
     expected-solution-2))
(define (test-day2-2)
  (and (test-day2-horizontal-2)
       (test-day2-depth-2)
       (test-day2-sub-product-2)))


(define (run-day2 f piloting-mode-h piloting-mode-d)
  (sub-hd (pilot-sub (make-sub 0 0 0)
		     piloting-mode-h
		     piloting-mode-d
		     (map (lambda (s)
			    (define (convert-last-to-number l)
			      (list (car l)
				    (string->number (cadr l))))
			    (convert-last-to-number (string-split s #\space)))
			  (read-file f)))))

(define (run-day2-1 f)
  (run-day2 f
	    piloting-mode-h-1
	    piloting-mode-d-1))
(define (run-day2-2 f)
  (run-day2 f
	    piloting-mode-h-2
	    piloting-mode-d-2))
