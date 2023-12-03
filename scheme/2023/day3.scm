;;;; day3 --- Gear ratios
;;

;;; Part 1
;;
;; The engineer explains that an engine part seems to be missing from the engine, but nobody can
;; figure out which one. If you can add up all the part numbers in the engine schematic, it should
;; be easy to work out which part is missing.
;;
;; The engine schematic (your puzzle input) consists of a visual representation of the engine. There
;; are lots of numbers and symbols you don't really understand, but apparently any number adjacent
;; to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.)
;; do not count as a symbol.)
;;
;; Here is an example engine schematic:
;; ```
;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..
;; ```
;; In this schematic, two numbers are not part numbers because they are not adjacent to a symbol:
;; 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a
;; part number; their sum is 4361.

(define example-input
  '("467..114.."
    "...*......"
    "..35..633."
    "......#..."
    "617*......"
    ".....+.58."
    "..592....."
    "......755."
    "...$.*...."
    ".664.598.."))

;; To find neighbours of a number we need to know what it's start and end index is, any symbol
;; appearing within the range start-1<=i<=end+1 on either the row above, the row of the number or
;; the row below is then adjacent.

(define (safe-string-index s pred start end)
  (if (and start
	   (< start (string-length s)))
      (string-index s pred start end)
      #f))

(define (find-number-start s start)
  (safe-string-index s char-numeric? start (string-length s)))
(define (find-number-end s start)
  (define (get-start s start)
    (let ((num-start (find-number-start s start)))
      (if num-start
	  num-start
	  (string-length s))))
  (define (run start end)
    (safe-string-index s
		       (lambda (c)
			 (not (char-numeric? c)))
		       start
		       end))
  (let ((idx_last (run (get-start s start)
		       (string-length s))))
    (if idx_last
	(- idx_last 1)
	(if (find-number-start s (- (string-length s) 1))
	    (- (string-length s) 1)
	    #f))))

;; Given index limits we can search a set of rows for adjacent values.
(use-modules (srfi srfi-1)) ; List processing module
(define (adjacency-in-row? start end row)
  (if (safe-string-index row
			 (lambda (c)
			   (and (not (char-numeric? c))
				(not (equal? c #\.))))
			 (max (- start 1) 0)
			 (min (+ (+ end 1) 1) ;; String search below end
			      (string-length row)))
      #t
      #f))
(define (adjacency? start end rows)
  (reduce (lambda (a b)
	    (or a b))
	  #f
	  (map (lambda (r)
		 (adjacency-in-row? start end r))
	       rows)))

;; Find numbers in a row with adjacency
(define (get-row-numbers prev row next)
  (define (run lst start end)
    (if (or (and (not start) (not end))
	    (when start
	      (>= start (string-length row))))
	lst
	(if (adjacency? start end (list prev row next))
	    (run (append lst
			 (list (string->number (substring row
							  start
							  (+ end 1)))))
		 (find-number-start row (+ end 1))
		 (find-number-end row (+ end 1)))
	    (run lst
		 (find-number-start row (+ end 1))
		 (find-number-end row (+ end 1))))))
  (run '()
       (find-number-start row 0)
       (find-number-end row (find-number-start row 0))))

(define (find-part-numbers data)
  (define (run lst prev row data)
    (if (null? data)
	(append lst (get-row-numbers prev
				     row
				     ""))
	(run (append lst (get-row-numbers prev
					  row
					  (car data)))
	     row
	     (car data)
	     (cdr data))))
  (run '()
       ""
       (car data)
       (cdr data)))

(define (day3.1 data)
  (reduce +
	  0
	  (find-part-numbers data)))

;;; Tests

(use-modules (srfi srfi-64))

(test-begin "find-number-start_end")

(test-equal 0 (find-number-start "467..114.." 0))
(test-equal 1 (find-number-start "467..114.." 1))
(test-equal 2 (find-number-start "467..114.." 2))
(test-equal 5 (find-number-start "467..114.." 3))
(test-equal 5 (find-number-start "467..114.." 4))
(test-equal 5 (find-number-start "467..114.." 5))
(test-equal 6 (find-number-start "467..114.." 6))
(test-equal 7 (find-number-start "467..114.." 7))
(test-equal #f (find-number-start "467..114.." 8))
(test-equal #f (find-number-start "467..114.." 9))

(test-equal 2 (find-number-end "467..114.." 0))
(test-equal 2 (find-number-end "467..114.." 1))
(test-equal 2 (find-number-end "467..114.." 2))
(test-equal 7 (find-number-end "467..114.." 3))
(test-equal 7 (find-number-end "467..114.." 4))
(test-equal 7 (find-number-end "467..114.." 5))
(test-equal 7 (find-number-end "467..114.." 6))
(test-equal 7 (find-number-end "467..114.." 7))
(test-equal #f (find-number-end "467..114.." 8))
(test-equal #f (find-number-end "467..114.." 9))

(test-equal #f (find-number-start "...*......" 0))
(test-equal #f (find-number-end "...*......" 0))

(test-equal 8 (find-number-start "..35..633" 8))
(test-equal 8 (find-number-end "..35..633" 6))

(test-end "find-number-start_end")

(test-begin "adjacency?")

(test-equal #f (adjacency-in-row? 0 2 "467..114.."))
(test-equal #f (adjacency-in-row? 5 7 "467..114.."))
(test-equal #t (adjacency-in-row? 0 2 "...*......"))
(test-equal #f (adjacency-in-row? 5 7 "...*......"))

(test-equal #t (adjacency? 0 2 '(""
				 "467..114.."
				 "...*......")))
(test-equal #f (adjacency? 5 7 '(""
				 "467..114.."
				 "...*......")))
(test-equal #f (adjacency? #f #f '("467..114.."
				   "...*......"
				   "..35..633.")))

(test-end "adjacency?")

(test-begin "row-numbers")

(test-equal '(467) (get-row-numbers ""
				    "467..114.."
				    "...*......"))
(test-equal '() (get-row-numbers "467..114.."
				 "...*......"
				 "..35..633."))

(test-end "row-numbers")

(test-begin "day3-part1")

(test-equal '(467 35 633 617 592 755 664 598)
  (find-part-numbers example-input))
(test-equal 4361 (day3.1 example-input))

(test-end "day3-part1")
