(ql:quickload :cl-ppcre)

(load "mwlib.lisp")

(defun parse-mask (mask)
  (let ((leave-mask (parse-integer (tr mask '((#\1 #\0) (#\X #\1))) :radix 2))
	(set-mask (parse-integer (tr mask '((#\X #\0))) :radix 2)))
    (cons leave-mask set-mask)))

(defun apply-mask (mask val)
  (logior (logand val (car mask))
	  (logand (lognor 0 (car mask)) (cdr mask))))

(defun update-mem (mem key val mask)
  (setf (gethash key mem)
	(apply-mask mask val)))

(defun process-lines (lines mem mask parse-mask-func update-func)
  (if (null lines) mem
      (let ((line (car lines)))	    
	(ppcre:register-groups-bind (new-mask)
				    ("mask = (.*)" line)
	  (process-lines (cdr lines) mem (funcall parse-mask-func new-mask)
			 parse-mask-func update-func)
				    )
	(ppcre:register-groups-bind ((#'parse-integer  key val))
	    ("mem\\[([^]]*)\\] = (.*)" line)
	  (funcall update-func mem key val mask)
	  (process-lines (cdr lines) mem mask parse-mask-func update-func)))))

(defun day14a ()
  (let* ((lines (read-file "day14.txt"))
	 (mem (make-hash-table)))
    (process-lines lines mem 0 #'parse-mask #'update-mem)
    (loop for b being the hash-value of mem
	 sum b)))

(defun parse-mask-b (mask)
  (let ((leave-mask (parse-integer (tr mask '((#\X #\0) (#\1 #\0) (#\0 #\1))) :radix 2))
	(set-mask (parse-integer (tr mask '((#\X #\0))) :radix 2))
	(float-mask (parse-integer (tr mask '((#\X #\1) (#\1 #\0))) :radix 2)))
    (list leave-mask set-mask float-mask)))

(defun leftmost-bit-1 (m)
  (if (= m 0) 1
      (* 2 (leftmost-bit-1 (floor (/ m 2))))))

(defun leftmost-bit (m)
  (floor (/ (leftmost-bit-1 m) 2)))

(defun compute-floats (float-mask b)
  (if (= 0 float-mask) (list b)
      (let ((leftmost-bit (leftmost-bit float-mask)))
	(append (compute-floats (- float-mask leftmost-bit) b)
		(compute-floats (- float-mask leftmost-bit) (+ b leftmost-bit))))))

(defun apply-mask-b (mask val)
  (let ((leave-mask (first mask))
	(set-mask (second mask))
	(float-mask (third mask)))
    (compute-floats float-mask (logior (logand leave-mask val)
				       set-mask))))

(defun update-mem-b (mem key val mask)
  (let ((updates (apply-mask-b mask key)))
    (mapcar (lambda (addr) (setf (gethash addr mem) val)) updates)))

(defun day14b ()
  (let* ((lines (read-file "day14.txt"))
	 (mem (make-hash-table)))
    (process-lines lines mem 0 #'parse-mask-b #'update-mem-b)
    (loop for b being the hash-value of mem
	 sum b)))
