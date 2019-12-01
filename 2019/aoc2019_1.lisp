;;;
;;
;;; LICENSE
;;
;; aoc2019_1.lisp - solution to the 2019 Advent of Code day 1 challenge.
;; Copyright (C) 2019  Paul Bartholomew
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; CODE
;;

(defun total-fuel (module-masses)
  (labels ((compute-total-fuel (module-masses fuel)
			       (if (car module-masses)
				   (compute-total-fuel (cdr module-masses)
						       (+ fuel
							  (calc-module-fuel (car module-masses))))
				 fuel))))
  (compute-total-fuel module-masses 0))

(defun calc-module-fuel (module-mass)
  (- (floor (/ module-mass 3)) 2))
