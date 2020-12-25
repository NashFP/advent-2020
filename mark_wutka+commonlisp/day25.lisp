(load "mwlib.lisp")

(defun do-transform (subject-number loop-size)
  (labels ((do-transform-rec (val loop-size)
	     (if (= 0 loop-size) val
		 (do-transform-rec (mod (* val subject-number) 20201227)
		   (1- loop-size)))))
    (do-transform-rec 1 loop-size)))

(defun compute-loop-size (public-key)
  (labels ((try-loop-size (val size)
	     (if (= val public-key) size
		 (try-loop-size (mod (* val 7) 20201227) (1+ size)))))
    (try-loop-size 7 1)))

(defun day25a ()
  (let* ((keys (mapcar #'parse-integer (read-file "day25.txt")))
	 (card-key (first keys))
	 (card-loop-size (compute-loop-size card-key))
	 (door-key (second keys))
	 (door-loop-size (compute-loop-size door-key))
	 (key1 (do-transform card-key door-loop-size))
	 (key2 (do-transform door-key card-loop-size)))
    (if (= key1 key2)
	key1
	(format t "Can't find encryption key~%"))))

