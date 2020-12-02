
(define (day1 datafile target-sum f)
  (define (run data)
    (define d1 (car data))
    (define d2 (car (cdr data)))
    (if (= target-sum
	   (+ d1 d2))
	(f d1 d2)
	(run (cdr data))))
  (run (parse-file-to-list datafile string->number)))
