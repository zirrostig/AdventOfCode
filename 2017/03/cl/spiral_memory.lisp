;;;; spiral_memory.lisp

;;; --- Day 3: Spiral Memory ---
;;; You come across an experimental new kind of memory stored on an
;;; infinite two-dimensional grid.
;;;
;;; Each square on the grid is allocated in a spiral pattern starting
;;; at a location marked 1 and then counting up while spiraling
;;; outward. For example, the first few squares are allocated like
;;; this:
;;;
;;; 37 36 35 34 33 32 21
;;; 38 17 16 15 14 13 30
;;; 39 18  5  4  3 12 29
;;; 40 19  6  1  2 11 28
;;; 41 20  7  8  9 10 27
;;; 42 21 22 23 24 25 26
;;; 43 44 45 46 47 -> ..
;;;
;;; While this is very space-efficient (no squares are skipped),
;;; requested data must be carried back to square 1 (the location of
;;; the only access port for this memory system) by programs that can
;;; only move up, down, left, or right. They always take the shortest
;;; path: the Manhattan Distance between the location of the data and
;;; square 1.
;;;
;;; For example:
;;;
;;; * Data from square 1 is carried 0 steps, since it's at the access
;;;   port.
;;; * Data from square 12 is carried 3 steps, such as: down, left,
;;;   left.
;;; * Data from square 23 is carried only 2 steps: up twice.
;;; * Data from square 1024 must be carried 31 steps.
;;;
;;; How many steps are required to carry the data from the square
;;; identified in your puzzle input all the way to the access port?

(defun ring-index (val)
  "Return ring index. This is the index of the largest odd number from
  a list of odds, that satisfies (< (expt ODD 2) (sqrt VAL)).
  If VAL is 1, then 0 is returned."
  (floor (ceiling (sqrt val)) 2))

(defun ring-side-length (ring)
  "Return the length of a ring's side given its index, RING"
  (+ 1 (* 2 ring)))

(defun ring-first-orthogonal (ring)
  "Return the value in the ring that is directly to the right of 1"
  (+ ring (expt (- (* 2 ring) 1) 2)))

(defun distance (val)
  "Calculates the manhattan distance to 1. This is done in two parts,
  first the ring index is calculated, this is the minimum distance for
  a number in that ring. Then the minimum offset from an orthogonal
  (one of 4 values in a ring with the minimum distance) is
  calculated. These two numbers are then added together to get the
  final distance."
  (if (< val 2) 0
      (let* ((ring (ring-index val))
	     (from-ortho (abs (- val (ring-first-orthogonal ring))))
	     (mod-side (mod from-ortho (* ring 2))))
	(+ ring (if (> mod-side ring)
		    (- ring (- mod-side ring))
		    mod-side)))))

(defun main()
  (loop for line = (read-line *standard-input* nil nil)
     while line ; stop on EOF
     for val = (parse-integer line)
     do (format t "~D~%" (distance val))))

(main)
