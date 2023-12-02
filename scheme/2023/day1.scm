;;;; day1.scm --- Trebuchet?!
;;
;;; Part 1
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

(define (day1:parse-line f str)
  "Parse a line of input, taking an alphanumeric string and returning the integer formed by
combining the first and last numerical characters."
  (list->number (first-and-last (string->numeric-string (f str)))))

(define (day1 f lst)
  (define (run sum lst)
    (if (not (null? lst))
	(run (+ sum (day1:parse-line f (car lst)))
	     (cdr lst))
	sum))
  (run 0 lst))

(define (day1.1 lst)
  (day1 identity lst))

;; Result: 54605

;;; Part 2
;;
;; Your calculation isn't quite right. It looks like some of the digits are actually spelled out
;; with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid
;; "digits".
;;
;; Equipped with this new information, you now need to find the real first and last digit on each
;; line. For example:
;; ```
;; two1nine
;; eightwothree
;; abcone2threexyz
;; xtwone3four
;; 4nineeightseven2
;; zoneight234
;; 7pqrstsixteen
;; ```
;;
;; In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together
;; produces 281.

;; Building on the Part 1 solution, a first pass that substitutes names for digits will allow the
;; same code to be reused.
;;
;; Note the edge case when numbers alias, e.g. oneight -> 18.

(define alphanums '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(define (alpha->numeric str)
  "If the first characters of a string name a number, replace the first character with the
corresponding digit, e.g. 'onehjkl'->'1nehjkl', otherwise returns the original string."
  (define (run alphanums ctr)
    "Search alphanums for matching name string."
    (if (not (null? alphanums))
	(let ((nch (min (string-length str)
			(string-length (car alphanums)))))
	  (if (string=? (car alphanums)
			(string-take str nch))
	      (string-append (number->string ctr)
			     (string-drop str 1))
	      (run (cdr alphanums) (+ ctr 1))))
	str))
  (run alphanums 0))

(define (substitute-alpha->numeric str)
  "Replace any named numbers appearing in a string with digits."
  (define (run out in)
    (if (= (string-length in) 0)
	out
	(let ((newstr (alpha->numeric in)))
	  (run (string-append out (string-take newstr 1))
	       (string-drop newstr 1)))))
  (run "" str))

(define (day1.2 lst)
  (day1 substitute-alpha->numeric lst))

;; Result: 55429

;;; Tests

(use-modules (srfi srfi-64))

;; Test filtering a string to only its numeric characters using the example inputs.
(test-begin "string->numeric-string")

(test-equal "12" (string->numeric-string "1abc2"))
(test-equal "38" (string->numeric-string "pqr3stu8vwx"))
(test-equal "12345" (string->numeric-string "a1b2c3d4e5f"))
(test-equal "7" (string->numeric-string "treb7uchet"))

(test-end "string->numeric-string")

;; Test processing the Part 1 example inputs.
(test-begin "day1.1-examples")

(test-equal '(#\1 #\2) (first-and-last "12"))
(test-equal '(#\3 #\8) (first-and-last "38"))
(test-equal '(#\1 #\5) (first-and-last "12345"))
(test-equal '(#\7 #\7) (first-and-last "7"))

(test-equal 12 (list->number (string->list "12")))
(test-equal 38 (list->number (string->list "38")))
(test-equal 12345 (list->number (string->list "12345")))
(test-equal 7 (list->number (string->list "7")))

(test-equal 12 (day1:parse-line identity "12"))
(test-equal 38 (day1:parse-line identity "pqr3stu8vwx"))
(test-equal 15 (day1:parse-line identity "a1b2c3d4e5f"))
(test-equal 77 (day1:parse-line identity "treb7uchet"))

(test-equal 142 (day1 identity
		      '("1abc2"
			"pqr3stu8vwx"
			"a1b2c3d4e5f"
			"treb7uchet")))

(test-end "day1.1-examples")

;; Test converting alpha->numeric, e.g. "zero"->"0", "one"->"1" ... "nine"->"9", "foo"->"foo"
(test-begin "alpha->numeric")

(test-equal "0ero" (alpha->numeric "zero"))
(test-equal "1ne" (alpha->numeric "one"))
(test-equal "2wo" (alpha->numeric "two"))
(test-equal "3hree" (alpha->numeric "three"))
(test-equal "4our" (alpha->numeric "four"))
(test-equal "5ive" (alpha->numeric "five"))
(test-equal "6ix" (alpha->numeric "six"))
(test-equal "7even" (alpha->numeric "seven"))
(test-equal "8ight" (alpha->numeric "eight"))
(test-equal "9ine" (alpha->numeric "nine"))
(test-equal "foo" (alpha->numeric "foo"))
(test-equal "1nehjkl" (alpha->numeric "onehjkl"))

(test-end "alpha->numeric")

;; Test substitution of the part2 examples
(test-begin "day1.2-examples")

(test-equal "2wo19ine" (substitute-alpha->numeric "two1nine"))
(test-equal "8igh2wo3hree" (substitute-alpha->numeric "eightwothree"))
(test-equal "abc1ne23hreexyz" (substitute-alpha->numeric "abcone2threexyz"))
(test-equal "x2w1ne34our" (substitute-alpha->numeric "xtwone3four"))
(test-equal "49ine8ight7even2" (substitute-alpha->numeric "4nineeightseven2"))
(test-equal "z1n8ight234" (substitute-alpha->numeric "zoneight234"))
(test-equal "7pqrst6ixteen" (substitute-alpha->numeric "7pqrstsixteen"))

(test-equal 281 (day1 substitute-alpha->numeric
		      '("two1nine"
			"eightwothree"
			"abcone2threexyz"
			"xtwone3four"
			"4nineeightseven2"
			"zoneight234"
			"7pqrstsixteen")))

(test-end "day1.2-examples")
