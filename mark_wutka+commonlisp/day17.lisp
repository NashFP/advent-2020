(load "mwlib.lisp")

(defun add-offset (v off)
  (map 'vector #'+ v off))

(defun initialize-space (lines dimensions)
  (let ((space-map (make-hash-table :test #'equalp)))
    (loop for line in lines
       for y from 0
       do (map 'list
	       (lambda (ch x)
		 (setf (gethash (apply #' vector x y (repeat 0 (- dimensions 2))) space-map) ch))
	       line (iota (length line))))
    space-map))

(defun count-adjacent-active (v space-map neighbors)
  (apply #'+ (mapcar (lambda (off) 
		       (if (equal #\# (gethash (add-offset v off) space-map)) 1 0))
		     neighbors)))

(defun update-cell (v space-map new-map neighbors)
  (if (not (gethash v new-map))
      (let ((count (count-adjacent-active v space-map neighbors)))
	(cond ((and (not (equal (gethash v space-map) #\#))
		    (= count 3)) (setf (gethash v new-map) #\#))
	      ((and (equal (gethash v space-map) #\#) (>= count 2) (<= count 3))
	       (setf (gethash v new-map) #\#))
	      (t nil)))))

(defun do-round (space-map neighbors)
  (let ((new-map (make-hash-table :test #'equalp)))
    (loop for v being the hash-keys of space-map
       do (update-cell v space-map new-map neighbors)
	 (mapcar (lambda (off)
		   (update-cell (add-offset v off) space-map new-map neighbors))
		 neighbors))
    new-map))

(defun do-rounds (n space-map neighbors)
  (if (= n 0) space-map
      (do-rounds (1- n) (do-round space-map neighbors) neighbors)))

(defun count-all-active (space-map)
  (loop for v being the hash-values of space-map
     when (equal v #\#)
       count 1))

(defun prod (a b)
  (mapcan (lambda (bx) (mapcar (lambda (ax)
				 (if (listp bx) (cons ax bx)
				     (list ax bx))) a)) b))
(defun compute-neighbors (n)
  (mapcar (lambda (o) (apply #'vector o))
	  (remove (repeat 0 n)
		  (reduce #'prod (repeat '(-1 0 1) n) :from-end t) :test #'equalp)))

(defun day17a ()
  (let ((space-map (initialize-space (read-file "day17.txt") 3)))
    (count-all-active (do-rounds 6 space-map (compute-neighbors 3)))))

(defun day17b ()
  (let ((space-map (initialize-space (read-file "day17.txt") 4)))
    (count-all-active (do-rounds 6 space-map (compute-neighbors 4)))))
