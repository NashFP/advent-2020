(ql:quickload :cl-ppcre)
(load "mwlib.lisp")

;;; This is a modified implementation of the second part of day 13
;;; Peter Siebel did a mathy solution to compute what I was doing
;;; iteratively. This file contains my original solution, Peter's solution,
;;; and then my solution with part of the iteration replaced with
;;; a mathy solution. Run (day13b-timed) to see the timings for each

;;; Here is an example run:
;;;My original solution:
;;;Evaluation took:
;;;  0.000 seconds of real time
;;;  0.000098 seconds of total run time (0.000088 user, 0.000010 system)
;;;  100.00% CPU
;;;  221,946 processor cycles
;;;  0 bytes consed
;;;  
;;;My modified solution:
;;;Evaluation took:
;;;  0.000 seconds of real time
;;;  0.000036 seconds of total run time (0.000036 user, 0.000000 system)
;;;  100.00% CPU
;;;  95,305 processor cycles
;;;  0 bytes consed
;;;  
;;;My adaptation of Peter Siebel's solution:
;;;Evaluation took:
;;;  0.000 seconds of real time
;;;  0.000040 seconds of total run time (0.000040 user, 0.000000 system)
;;;  100.00% CPU
;;;  108,750 processor cycles
;;;  0 bytes consed

(defun get-buses (line)
  (let ((buses (ppcre:split "," line)))
    (mapcar #'parse-integer
	    (remove-if (lambda (x) (equal x "x")) buses))))

(defun early (depart-time t1)
  (- t1 (mod depart-time t1)))

(defun earliest-bus (depart-time buses)
  (reduce (lambda (t1 t2)
	    (if (< (early depart-time t1) (early depart-time t2)) t1 t2))
	  buses))

(defun day13a ()
  (let* ((lines (read-file "day13.txt"))
	 (depart-time (parse-integer (car lines)))
	 (buses (get-buses (cadr lines)))
	 (earliest (earliest-bus depart-time buses)))
    (* earliest (early depart-time earliest))))

(defun get-buses-and-offsets (line)
  (let ((buses (ppcre:split "," line)))
    (loop for b in buses
       for i from 0
       when (not (equal b "x"))
       collect (cons i (parse-integer b))
	)))

;;; Find the time (offset) at which time the bus that repeats every
;;; target-mod minutes is at the position (target-offset) relative
;;; to the offset.
(defun find-offset (offset period target-mod target-offset)
  (if (= 0 (mod (+ offset target-offset) target-mod) )      
      offset
      (find-offset (+ offset period) period target-mod target-offset)))

;;; Offset is the time offset at which all the targets so far will reach
;;; their desired positions, and period is the amount of time it will
;;; take to repeat that pattern. We now calculate the offset needed for the
;;; next bus to be in the proper position (new-offset). The period is then
;;; multiplied by the target mod, which is the length of time it takes that
;;; bus cycle to repeat.
(defun find-offsets (offset period targets)
  (if (null targets) offset
      (let* ((target-offset (caar targets))
	     (target-mod (cdar targets))
	     (new-offset (find-offset offset period target-mod target-offset)))
	(find-offsets new-offset (* period target-mod) (cdr targets)))))


(defun day13b ()
  (let* ((lines (read-file "day13.txt"))
	 (targets (get-buses-and-offsets (cadr lines))))
    ;;; The initial offset is just the offset for the first bus, and
    ;;; the initial period is the time for the first bus to repeat
    (find-offsets (caar targets) (cdar targets) (cdr targets))))

;;; Extended gcd calculation translated from Peter Siebel's Python version at
;;; https://github.com/gigamonkey/advent-of-code-2020/blob/main/day-13.py
(defun extended-euclidian (a b)
  (if (= a 0) (values b 0 1)
      (multiple-value-bind (g x y) (extended-euclidian (mod b a) a)
	(values g (- y (* x (floor (/ b a)))) x))))

;;; Modular inverse (find x such that ax = 1 mod b) translated from
;;; Peter Siebel's Python version
(defun modular-inverse (a b)
  (multiple-value-bind (g x -) (extended-euclidian a b)
    (if (= g 1) (mod x b) (error (format nil "Not co-prime ~D ~D" a b)))))

;;; Compute component for linear congruence described at:
;;; https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
(defun calc-congruence (n period all-prod)
  (let ((rest-prod (/ all-prod period)))
    (* (modular-inverse rest-prod period) rest-prod (- period n)
       )))

(defun day13b-modinverse ()
  (let* ((lines (read-file "day13.txt"))
	 (targets (get-buses-and-offsets (second lines)))
	 (all-prod (apply #'* (mapcar #'cdr targets))))
    ;;; Compute x where x mod each target = the desired offset
    (mod (apply #'+ (mapcar (lambda (target) (calc-congruence (car target) (cdr target) all-prod))
			    targets))
	 all-prod)))

;;; My original solution used iteration to find out how many periods must elapse for the
;;; target bus to start at the correct offset. This version uses the modular inverse.
(defun find-offset2 (offset period target-mod target-offset)
  ;;; Start-offset is where the current offset is in the target bus cycle
  (let* ((start-offset (mod offset target-mod))
	 ;;; target-offset is where we need it to be in the bus cycle, if the bus must start
	 ;;; target-offset minutes after the current time t, then we actually want to compute
	 ;;; the place in the cycle that is target-offset minutes before that, which is the
	 ;;; same as target-mod - target-offset
	 (new-target-offset (- target-mod target-offset))
	 ;;; target-diff is then the difference between where we start and where we end
	 (target-diff (mod (- new-target-offset start-offset) target-mod)))
    ;;; We add to the offset the period times the number of cycles of period mod target-mod it
    ;;; will take for the bus start time to change by target-diff
    (+ offset (* period (mod (* target-diff (modular-inverse period target-mod)) target-mod)))))

(defun find-offsets2 (offset period targets)
  (if (null targets) offset
      (let* ((target-offset (caar targets))
	     (target-mod (cdar targets))
	     (new-offset (find-offset2 offset period target-mod target-offset)))
	(find-offsets2 new-offset (* period target-mod) (cdr targets)))))

;;; Try my original solution, my solution with the mathy find-offset, and then
;;; my adaptation of Peter Siebel's solution and print out the timing for each
(defun day13b-timed ()
  (let* ((lines (read-file "day13.txt"))
	 (targets (get-buses-and-offsets (cadr lines))))
    ;;; The initial offset is just the offset for the first bus, and
    ;;; the initial period is the time for the first bus to repeat
    (format t "My original solution:~%")
    (time (find-offsets (caar targets) (cdar targets) (cdr targets)))
    (format t "My modified solution:~%")
    (time (find-offsets2 (caar targets) (cdar targets) (cdr targets)))
    (format t "My adaptation of Peter Siebel's solution:~%")
    (time (let ((all-prod (apply #'* (mapcar #'cdr targets))))
	    (mod (apply #'+ (mapcar (lambda (target) (calc-congruence (car target) (cdr target) all-prod))
				    targets))
		 all-prod)))))
