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

;; Utilities for parsing a game
(define (game-split game)
  (string-split game #\:))
(define (get-observations game)
  (car (cdr (game-split game))))
(define (get-game-id game)
  (string->number (car (cdr (string-split (car (game-split game))
					  #\space)))))

;; General utilities
(define (list-max lst)
  "Find the maximum of a list, return null for the empty list."
  (define (run maxval lst)
    (if (null? lst)
	maxval
	(run (if (null? maxval)
		 (car lst)
		 (max maxval (car lst)))
	     (cdr lst))))
  (run '() lst))

(define (list-sum lst)
  "Sum the values in a list."
  (define (run sum lst)
    (if (null? lst)
	sum
	(run (+ sum (car lst))
	     (cdr lst))))
  (run 0 lst))

;; To test if a game was possible, the number of red/blue/green cubes must be >= the maximum
;; observed in that game.
(define (possible-game? game max-red max-blue max-green)
  "Check if a game was possible given maximum values of red, blue and green."
  (and (possible-colour? game "red" max-red)
       (possible-colour? game "blue" max-blue)
       (possible-colour? game "green" max-green)))

(define (possible-colour? game colour max-colour)
  "Determine if a game was possible for a given colour, given its maximum value."
  (let ((observed-max (colour-observed-max game colour)))
    (or (null? observed-max)
	(>= max-colour observed-max))))

(define (colour-observed-max game colour)
  "Find the maximum observed count of a colour in a game."
  (list-max (get-colour-counts colour (get-observations game))))

(define (get-colour-observations colour obs)
  "Get the observations of a specific colour from a set of all observations.

Observations are pairs of the form <count> <colour> and are separated by either commas or
semi-colons."
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
  "Get a list of the counts for a given colour from a set of observations."
  (map (lambda (ob)
	 (string->number (car (string-split ob #\space))))
       (get-colour-observations colour obs)))

(define example-input '("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
			"Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 read; 1 green, 1 blue"
			"Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
			"Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
			"Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(define (get-possible-ids games max-red max-blue max-green)
  "Find the game IDs of all possible games for a given set of colour maximum counts, if a given game
is not possible replace its ID with zero."
  (map (lambda (g)
	 (if (possible-game? g max-red max-blue max-green)
	     (get-game-id g)
	     0))
       games))

(define (day2 games max-red max-blue max-green)
  "Computes the sum of all valid game IDs for a given set of colour maximum counts."
  (list-sum (get-possible-ids games
			      max-red
			      max-blue
			      max-green)))

;;; Part 2
;;
;; As you continue your walk, the Elf poses a second question: in each game you played, what is the
;; fewest number of cubes of each color that could have been in the bag to make the game possible?
;;
;; Again consider the example games from earlier:
;; ```
;; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
;; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
;; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
;; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
;; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
;; ```
;;     In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If
;;     any color had even one fewer cube, the game would have been impossible.
;;     Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
;;     Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
;;     Game 4 required at least 14 red, 3 green, and 15 blue cubes.
;;     Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.
;;
;; The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied
;; together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560,
;; 630, and 36, respectively. Adding up these five powers produces the sum 2286.

;; This is fairly easy - already getting the maximum observation of each colour per game (equivalent
;; to the minimum required) so we just need to compute the product of these across all games and sum
;; the result!
(define (get-cube-powers games)
   (map (lambda (g)
	  (* (list-max (get-colour-counts "red" (get-observations g)))
	     (list-max (get-colour-counts "blue" (get-observations g)))
	     (list-max (get-colour-counts "green" (get-observations g)))))
	games))
(define (day2.2 games)
  (list-sum (get-cube-powers games)))
