(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun cartesian-product (l)
  (loop
     for x in l
     nconc (loop for y in l
	      collect (list x y))))

(defun cartesian-product3 (l)
  (loop
     for x in l
     nconc (loop for y in l
	      nconc (loop for z in l
		       collect (list x y z)))))

(defun day1a ()
  (let* ((numbers (mapcar #'parse-integer (read-file "day1a.txt")))
	 (numprod (cartesian-product numbers)))
    (apply #'* (car (remove-if-not (lambda (l) (= 2020 (apply #'+ l))) numprod)))))

(defun day1b ()
  (let* ((numbers (mapcar #'parse-integer (read-file "day1a.txt")))
	 (numprod (cartesian-product3 numbers)))
    (apply #'* (car (remove-if-not (lambda (l) (= 2020 (apply #'+ l))) numprod)))))

  

