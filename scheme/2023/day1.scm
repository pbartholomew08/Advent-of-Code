;;;; day1.scm --- Trebuchet?!
;;
;;; Problem description:
;;
;; The newly-improved calibration document consists of lines of text; each line originally contained
;; a specific calibration value that the Elves now need to recover. On each line, the calibration
;; value can be found by combining the first digit and the last digit (in that order) to form a
;; single two-digit number.
;;
;; For example:
;; ```
;; 1abc2
;; pqr3stu8vwx
;; a1b2c3d4e5f
;; treb7uchet
;; ```
;; In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these
;; together produces 142.

;;; Part 1
;;
;; To extract the first and last numbers in a string first filter the string down to only numerical
;; characters, then append the first and last to get the value.
;;
;; There is an edge-case when there is a single numerical character - it is both first AND last -
;; see the example `treb7uchet->77`.
;;
;; An additional edge case is when there are no numbers in the string, in this case it should
;; probably return 0.

(define (string->numeric-string str)
  "Returns a string filtered on numerical characters."
  (list->string (filter char-numeric?
			(string->list str))))

(define (first-and-last str)
  "Return a pair of the first and last characters of a string; for a single character string it is
both the first AND last character."
  (let ((strlst (string->list str)))
    (list (car strlst)
	  ;; Handle the case of a single-digit string - in which case it is ALSO the last number.
	  (if (not (null? (cdr strlst)))
	      (car (reverse strlst)) ; Get the last of the list
	      (car strlst)))))

(define (list->number lst)
  "Convert a list of numeric characters to a single number."
  (string->number (list->string lst)))

(define (day1:parse-line str)
  "Parse a line of input, taking an alphanumeric string and returning the integer formed by
combining the first and last numerical characters."
  (list->number (first-and-last (string->numeric-string str))))

(define (day1 lst)
  (define (run sum lst)
    (if (not (null? lst))
	(run (+ sum (day1:parse-line (car lst)))
	     (cdr lst))
	sum))
  (run 0 lst))

;; Result: 54605
;;; Tests

(use-modules (srfi srfi-64))

;; Test filtering a string to only its numeric characters using the example inputs.
(test-begin "string->numeric-string")

(test-equal "12" (string->numeric-string "1abc2"))
(test-equal "38" (string->numeric-string "pqr3stu8vwx"))
(test-equal "12345" (string->numeric-string "a1b2c3d4e5f"))
(test-equal "7" (string->numeric-string "treb7uchet"))

(test-end "string->numeric-string")

;; Test processing the example inputs.
(test-begin "day1-examples")

(test-equal '(#\1 #\2) (first-and-last "12"))
(test-equal '(#\3 #\8) (first-and-last "38"))
(test-equal '(#\1 #\5) (first-and-last "12345"))
(test-equal '(#\7 #\7) (first-and-last "7"))

(test-equal 12 (list->number (string->list "12")))
(test-equal 38 (list->number (string->list "38")))
(test-equal 12345 (list->number (string->list "12345")))
(test-equal 7 (list->number (string->list "7")))

(test-equal 12 (day1:parse-line "12"))
(test-equal 38 (day1:parse-line "pqr3stu8vwx"))
(test-equal 15 (day1:parse-line "a1b2c3d4e5f"))
(test-equal 77 (day1:parse-line "treb7uchet"))

(test-equal 142 (day1 '("1abc2"
			"pqr3stu8vwx"
			"a1b2c3d4e5f"
			"treb7uchet")))

(test-end "day1-examples")
