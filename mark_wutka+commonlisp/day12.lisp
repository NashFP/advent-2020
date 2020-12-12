
(defun parse-dir (d)
  (cons (aref d 0) (parse-integer (subseq d 1))))

(defun move (x y facing dirs)
  (if (null dirs) (list x y facing)
      (let ((dir (caar dirs))
	    (units (cdar dirs)))
	(case dir
	  (#\N (move x (+ y units) facing (cdr dirs)))
	  (#\S (move x (- y units) facing (cdr dirs)))
	  (#\E (move (+ x units) y facing (cdr dirs)))
	  (#\W (move (- x units) y facing (cdr dirs)))
	  (#\R (move x y (mod (+ units facing) 360) (cdr dirs)))
	  (#\L (move x y (mod (- (+ 360 facing) units) 360) (cdr dirs)))
	  (#\F (case facing
		 (0 (move x (+ y units) facing (cdr dirs)))
		 (90 (move (+ x units) y facing (cdr dirs)))
		 (180 (move x (- y units) facing (cdr dirs)))
		 (270 (move (- x units) y facing (cdr dirs)))))))))

(defun day12a ()
  (let* ((dirs (mapcar #'parse-dir (read-file "day12.txt")))
	 (result (move 0 0 90 dirs)))
    (+ (abs (car result)) (abs (cadr result)))))

(defun move-b (x y way-x way-y dirs)
  (if (null dirs) (list x y)
      (let ((dir (caar dirs))
	    (units (cdar dirs)))
	(case dir
	  (#\N (move-b x y way-x (+ way-y units) (cdr dirs)))
	  (#\S (move-b x y way-x (- way-y units) (cdr dirs)))
	  (#\E (move-b x y (+ way-x units) way-y (cdr dirs)))
	  (#\W (move-b x y (- way-x units) way-y (cdr dirs)))
	  (#\R (case units
		 (90 (move-b x y way-y (- way-x) (cdr dirs)))
		 (180 (move-b x y (- way-x) (- way-y) (cdr dirs)))
		 (270 (move-b x y (- way-y) way-x (cdr dirs)))))
	  (#\L (case units
		 (90 (move-b x y (- way-y) way-x (cdr dirs)))
		 (180 (move-b x y (- way-x) (- way-y) (cdr dirs)))
		 (270 (move-b x y way-y (- way-x) (cdr dirs)))))
	  (#\F (move-b (+ x (* way-x units)) (+ y (* way-y units)) way-x way-y (cdr dirs)))))))

(defun day12b ()
  (let* ((dirs (mapcar #'parse-dir (read-file "day12.txt")))
	 (result (move-b 0 0 10 1 dirs)))
    (+ (abs (car result)) (abs (cadr result)))))
