;;;; day2.scm --- Cube conundrum
;;

;;; Part 1
;;
;; The Elf explains that you've arrived at Snow Island and apologizes for the lack of snow. He'll be
;; happy to explain the situation, but it's a bit of a walk, so you have some time. They don't get
;; many visitors up here; would you like to play a game in the meantime?
;;
;; As you walk, the Elf shows you a small bag and some cubes which are either red, green, or
;; blue. Each time you play this game, he will hide a secret number of cubes of each color in the
;; bag, and your goal is to figure out information about the number of cubes.
;;
;; To get information, once a bag has been loaded with cubes, the Elf will reach into the bag, grab
;; a handful of random cubes, show them to you, and then put them back in the bag. He'll do this a
;; few times per game.
;;
;; You play several games and record the information from each game (your puzzle input). Each game
;; is listed with its ID number (like the 11 in Game 11: ...) followed by a semicolon-separated list
;; of subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).
;;
;; For example, the record of a few games might look like this:
;;
;; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
;; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
;; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
;; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
;; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
;;
;; In game 1, three sets of cubes are revealed from the bag (and then put back again). The first set
;; is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes;
;; the third set is only 2 green cubes.
;;
;; The Elf would first like to know which games would have been possible if the bag contained only
;; 12 red cubes, 13 green cubes, and 14 blue cubes?
;;
;; In the example above, games 1, 2, and 5 would have been possible if the bag had been loaded with
;; that configuration. However, game 3 would have been impossible because at one point the Elf
;; showed you 20 red cubes at once ; similarly, game 4 would also have been impossible because the
;; Elf showed you 15 blue cubes at once. If you add up the IDs of the games that would have been
;; possible, you get 8.

;; To test if a game was possible, the number of red/blue/green cubes must be >= the maximum
;; observed in that game.
(define (possible-game? game max-red max-blue max-green)
  (and (possible-colour? game "red" max-red)
       (possible-colour? game "blue" max-blue)
       (possible-colour? game "green" max-green)))

(define (possible-colour? game colour max-colour)
  (let ((observed-max (colour-observed-max game colour)))
    (or (null? observed-max)
	(>= max-colour observed-max))))

(define (colour-observed-max game colour)
  (list-max (get-colour-counts colour (get-observations game))))

(define (game-split game)
  (string-split game #\:))
(define (get-observations game)
  (car (cdr (game-split game))))
(define (get-game-id game)
  (string->number (car (cdr (string-split (car (game-split game))
					  #\space)))))

(define (get-colour-observations colour obs)
  (define (observations-split obs)
    (string-split obs
		  (lambda (ch)
		    (or (equal? ch #\;)
			(equal? ch #\,)))))
  ;; Use map to drop the 1st character in a colour's observation data (leading space).
  (map (lambda (ss)
	 (string-drop ss 1))
       (filter (lambda (ob)
		 (string-contains ob colour))
	       (observations-split obs))))

(define (get-colour-counts colour obs)
  (map (lambda (ob)
	 (string->number (car (string-split ob #\space))))
       (get-colour-observations colour obs)))

(define (list-max lst)
  (define (run maxval lst)
    (if (null? lst)
	maxval
	(run (if (null? maxval)
		 (car lst)
		 (max maxval (car lst)))
	     (cdr lst))))
  (run '() lst))

(define example-input '("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
			"Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 read; 1 green, 1 blue"
			"Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
			"Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
			"Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(define (get-possible-ids games max-red max-blue max-green)
  (map (lambda (g)
	 (if (possible-game? g max-red max-blue max-green)
	     (get-game-id g)
	     0))
       games))

(define (list-sum lst)
  (define (run sum lst)
    (if (null? lst)
	sum
	(run (+ sum (car lst))
	     (cdr lst))))
  (run 0 lst))

(define (day2 games max-red max-blue max-green)
  (list-sum (get-possible-ids games
			      max-red
			      max-blue
			      max-green)))
