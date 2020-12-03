(load "mwlib.lisp")

(defun day1a ()
  (let ((numbers (mapcar #'parse-integer (read-file "day1a.txt"))))
    (loop named outer
       for x in numbers
       do (loop for y in numbers
	     when (= 2020 (+ x y))
	     do (return-from outer (* x y))))))

(defun day1b ()
  (let ((numbers (mapcar #'parse-integer (read-file "day1a.txt"))))
    (loop named outer
       for x in numbers
       do (loop for y in numbers
	     do (loop for z in numbers
		   when (= 2020 (+ x y z))
		   do (return-from outer (* x y z)))))))
