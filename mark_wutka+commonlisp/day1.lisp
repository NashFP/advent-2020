(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun day1a ()
  (let ((numbers (mapcar #'parse-integer (read-file "day1a.txt"))))
    (car (loop
	    for x in numbers
	    nconc (loop for y in numbers
		     if (= 2020 (+ x y))
		       collect (* x y))))))

(defun day1b ()
  (let ((numbers (mapcar #'parse-integer (read-file "day1a.txt"))))
    (car (loop
	    for x in numbers
	    nconc (loop for y in numbers
		       nconc (loop for z in numbers
				if (= 2020 (+ x y z))
				collect (* x y z)))))))
    

  

