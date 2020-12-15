
(defun init-game (numbers)
  (let ((spoken (make-hash-table)))
    (loop for n in numbers
       for i from 1
       do (setf (gethash n spoken) i)
       finally (return spoken))))

(defun play-game (turn limit spoken last-spoken)
  (if (= turn limit) last-spoken
      (let ((lookup (gethash last-spoken spoken)))
	(setf (gethash last-spoken spoken) turn)
	(play-game (1+ turn) limit spoken
		   (if lookup
		       (- turn lookup)
		       0)))))

(defun day15a ()
  (let ((spoken (init-game '(1 0 16 5 17))))
    (play-game 6 2020 spoken 4)))

(defun day15b ()
  (let ((spoken (init-game '(1 0 16 5 17))))
    (play-game 6 30000000 spoken 4)))
