;;; Advent of Code 2021/day3 Scheme implementation.
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

;;; 2021/day3
;;;
;;; The sub's diagnostic information is returned as a binary number, encoding GAMMA and EPSILON as
;;; the most and least common bits at each position, e.g. given
;;;
;;;   00100 11110 10110 10111 10101 01111 00111 11100 10000 11001 00010 01010
;;;
;;; then GAMMA=10110=22 and EPSILON=01001=9.
;;; The power consumption is given by their product.
