(load "mwlib.lisp")

;;; Count the adjacent items in sorted l that are 1 or 3 apart,
;;; return the product of the sums
(defun count-1-3 (l n1 n3)
  (if (null (cdr l)) (* n1 n3)
      (let ((a (car l))
	    (b (cadr l)))
	(cond ((= (- b a) 1) (count-1-3 (cdr l) (1+ n1) n3))
	      ((= (- b a) 3) (count-1-3 (cdr l) n1 (1+ n3)))
	      (t (count-1-3 (cdr l) n1 n3))))))

(defun day10a ()
  (let* ((adapters (mapcar #'parse-integer (read-file "day10.txt")))
	 ;;; Add 0 for the socket and max+3 for the end device, then sort the list
	 (sorted-adapters (sort (cons 0 (cons (+ 3 (apply #'max adapters)) adapters)) #'<)))
    (count-1-3 sorted-adapters 0 0)))

;;; Returns a list of adapters that are within 3 of the current adapter
(defun possible-nexts (curr-val adapter-num adapters)
  ;;; If adapter-num is outside the range of adapters, return the empty list
  (if (>= adapter-num (length adapters)) '()
      (let ((v (aref adapters adapter-num)))
	;;; If the diff is > 3, return the empty list, no more adapters qualify
	(if (> (- v curr-val) 3) '()
	    ;;; Otherwise, prepend this adapter to the list of other adapters
	    ;;; that qualify
	    (cons adapter-num (possible-nexts curr-val (1+ adapter-num) adapters))
	    ))))

(defun count-possibilities (adapter-num adapters possibilities)
  ;;; If adapter num is the last adapter, just return 1
  (if (= adapter-num (- (length adapters) 1)) 1
      ;;; Check to see if the possibilities for this adapter have already been computed
      (let ((adapter-poss (aref possibilities adapter-num)))
	(if (not (null adapter-poss)) adapter-poss
	    ;;; If not computed, get the list of adapters that can connect to this one
	    ;;; Compute their possible combinations to the end device, then sum those
	    (let* ((nexts (possible-nexts (aref adapters adapter-num) (1+ adapter-num) adapters))
		   (poss-list (mapcar
			       (lambda (an) (count-possibilities an adapters possibilities))
			       nexts)))
    	      ;;; Compute the sum and cache it (the return value of setf is the value
	      ;;; we are storing, so we can use it for the return value from the function
	      (setf (aref possibilities adapter-num) (apply #'+ poss-list)))
	    ))))

(defun day10b ()
  (let* ((adapters (mapcar #'parse-integer (read-file "day10.txt")))
	 ;;; Add 0 for the socket and max+3 for the end device, and convert the list
	 ;;; to a vector
	 (adapters-with-endpoints
	  (map 'vector #'identity (cons 0 (cons (+ 3 (apply #'max adapters)) adapters))))
	 ;;; Sort the adapters
	 (sorted-adapters (sort adapters-with-endpoints #'<))
	 ;;; Create a cache for the possibilities at each adapter position
	 (possibilities (make-array (length sorted-adapters) :initial-element nil)))
    (count-possibilities 0 sorted-adapters possibilities)))
