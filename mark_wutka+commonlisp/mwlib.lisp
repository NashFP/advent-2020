(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun split-groups (lines groups group)
  (cond ((null lines) (if (null group) groups (cons group groups)))
	((= (length (car lines)) 0) (split-groups (cdr lines) (cons group groups) '()))
	(t (split-groups (cdr lines) groups (cons (car lines) group)))))

(defun take (n list)
  (if (= n 0) '()
      (cons (car list) (take (1- n) (cdr list)))))

(defun take-while (f l)
  (if (null l) '()
      (if (apply f (car l)) (cons (car l) (take-while f (cdr l)))
	  (take-while f (cdr l)))))

(defun drop (n list)
  (if (= n 0) list
      (drop (1- n) (cdr list))))
