(ql:quickload :cl-ppcre)
(load "mwlib.lisp")

(defun get-buses (line)
  (let ((buses (ppcre:split "," line)))
    (mapcar #'parse-integer
	    (remove-if (lambda (x) (equal x "x")) buses))))

(defun early (depart-time t1)
  (- t1 (mod depart-time t1)))

(defun earliest-bus (depart-time buses)
  (reduce (lambda (t1 t2)
	    (if (< (early depart-time t1) (early depart-time t2)) t1 t2))
	  buses))

(defun day13a ()
  (let* ((lines (read-file "day13.txt"))
	 (depart-time (parse-integer (car lines)))
	 (buses (get-buses (cadr lines)))
	 (earliest (earliest-bus depart-time buses)))
    (* earliest (early depart-time earliest))))

(defun get-buses-and-offsets (line)
  (let ((buses (ppcre:split "," line)))
    (loop for b in buses
       for i from 0
       when (not (equal b "x"))
       collect (cons i (parse-integer b))
	)))

(defun find-offset (offset period target-mod target-offset)
  (if (= 0 (mod (+ offset target-offset) target-mod) )      
      offset
      (find-offset (+ offset period) period target-mod target-offset)))

(defun find-offsets (offset period targets)
  (if (null targets) offset
      (let* ((target-offset (caar targets))
	     (target-mod (cdar targets))
	     (new-offset (find-offset offset period target-mod target-offset)))
	(find-offsets new-offset (* period target-mod) (cdr targets)))))

(defun day13b ()
  (let* ((lines (read-file "day13.txt"))
	 (targets (get-buses-and-offsets (cadr lines))))
    (find-offsets (caar targets) (cdar targets) (cdr targets))))
