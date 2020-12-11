
(load "mwlib.lisp")

(defvar rc-offsets '((-1 . -1) (0 . -1) (1 . -1)
		     (-1 . 0)           (1 . 0)
		     (-1 . 1)  (0 . 1)  (1 . 1)))

(defun is-valid (rc row-max col-max)
  (let ((row (car rc))
	(col (cdr rc)))
    (and (>= row 0) (< row row-max)
	 (>= col 0) (< col col-max))))

(defun add-offset (rc rc-offset)
  (cons (+ (car rc) (car rc-offset))
	(+ (cdr rc) (cdr rc-offset))))

(defun get-adjacent-rc (rc seat-map)
  (let ((row-max (length seat-map))
	(col-max (length (aref seat-map 0))))
    (loop for rc-offset in rc-offsets
       if (is-valid (add-offset rc rc-offset) row-max col-max)
	 collect (add-offset rc rc-offset))))

(defun get-at-rc (rc seat-map)
  (aref (aref seat-map (car rc)) (cdr rc)))

(defun set-at-rc (rc seat-map val)
  (setf (aref (aref seat-map (car rc)) (cdr rc)) val))

(defun get-adjacent (rc seat-map)
  (let ((adjacent-rc (get-adjacent-rc rc seat-map)))
    (mapcar (lambda (rc) (get-at-rc rc seat-map)) adjacent-rc)))

(defun count-adjacent-occupied (rc seat-map)
  (length (remove-if-not (lambda (c) (eq c #\#)) (get-adjacent rc seat-map))))

(defun make-seat-grid (lines)
  (map 'vector (lambda (l) (make-array (length l) :initial-contents l)) lines))

(defun next-round-value-a (rc seat-map)
  (let ((current-val (get-at-rc rc seat-map)))
    (cond ((eq current-val #\.) #\.)
	  ((eq current-val #\L)
	   (if (= 0 (count-adjacent-occupied rc seat-map)) #\# #\L))
	  ((eq current-val #\#)
	   (if (>= (count-adjacent-occupied rc seat-map) 4) #\L #\#)))))

(defun new-seat-map (seat-map)
  (make-array (length seat-map) :initial-contents
	      (loop for row across seat-map
		   collect (make-array (length row) :initial-element #\.))))

(defun do-round (seat-map next-round-func)
  (let ((row-max (length seat-map))
	(col-max (length (aref seat-map 0)))
	(new-map (new-seat-map seat-map)))
    (dotimes (row row-max)
      (dotimes (col col-max)
	(set-at-rc (cons row col) new-map
		   (funcall next-round-func (cons row col) seat-map))
	))
    new-map))

(defun do-until-stable (seat-map next-round-func)
  (let ((new-map (do-round seat-map next-round-func)))
    (if (equalp seat-map new-map) seat-map
	(do-until-stable new-map next-round-func))))

(defun count-occupied (seat-map)
  (apply #'+
	 (map 'list (lambda (r) (length
				 (remove-if-not (lambda (c) (eq c #\#)) r))) seat-map)))

(defun day11a ()
  (let ((seat-map (make-seat-grid (read-file "day11.txt"))))
    (count-occupied (do-until-stable seat-map #'next-round-value-a))))

(defun get-at-offset-long (rc offset seat-map)
  (let ((row-max (length seat-map))
	(col-max (length (aref seat-map 0)))
	(rc-in-dir (add-offset rc offset)))
    (if (not (is-valid rc-in-dir row-max col-max)) nil
	(let ((seat-val (get-at-rc rc-in-dir seat-map)))
	  (if (or (eq seat-val #\L) (eq seat-val #\#)) seat-val
	      (get-at-offset-long rc-in-dir offset seat-map))))))

(defun count-adjacent-occupied-long (rc seat-map)
  (length (remove-if-not (lambda (c) (eq c #\#))
			 (mapcar (lambda (off) (get-at-offset-long rc off seat-map)) rc-offsets))))

(defun next-round-value-b (rc seat-map)
  (let ((current-val (get-at-rc rc seat-map)))
    (cond ((eq current-val #\.) #\.)
	  ((eq current-val #\L)
	   (if (= 0 (count-adjacent-occupied-long rc seat-map)) #\# #\L))
	  ((eq current-val #\#)
	   (if (>= (count-adjacent-occupied-long rc seat-map) 5) #\L #\#)))))

(defun print-grid (seat-map)
     (loop for r across seat-map
	do (format t "~{~C~}~%" (map 'list #'identity r))))

(defun day11b ()
  (let ((seat-map (make-seat-grid (read-file "day11.txt"))))
    (count-occupied (do-until-stable seat-map #'next-round-value-b))))
