(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun split-groups (lines groups group)
  (cond ((null lines) (if (null group) groups (cons group groups)))
	((= (length (car lines)) 0) (split-groups (cdr lines) (cons group groups) '()))
	(t (split-groups (cdr lines) groups (cons (car lines) group)))))

