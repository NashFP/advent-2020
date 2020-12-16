
(ql:quickload :cl-ppcre)
(ql:quickload :cl-match)
(load "mwlib.lisp")

(defun parse-range (range)
  (mapcar #'parse-integer (ppcre:split "-" range)))

(defun parse-ranges (ranges)
  (mapcar #'parse-range (ppcre:split " or " ranges)))

(defun parse-field-spec (line)
  (let* ((parts (ppcre:split ": *" line))
	 (tag (car parts))
	 (ranges (cadr parts)))
    (cons tag (parse-ranges ranges))))

(defun parse-num-list (line)
  (mapcar #'parse-integer (ppcre:split "," line)))

(defun check-range (val field-spec)
  (some (lambda (r) (and (>= val (car r)) (<= val (cadr r)))) (cdr field-spec)))

(defun valid-in-any-spec (val field-specs)
  (some (lambda (field-spec) (check-range val field-spec)) field-specs))

(defun invalid-values (ticket field-specs)
  (reduce #'append
	  (loop for field-val in ticket
	     when (not (valid-in-any-spec field-val field-specs))
	     collect field-val)))

(defun remove-invalid-values (tickets field-specs)
  (remove-if (lambda (ticket) (invalid-values ticket field-specs)) tickets))

(defun day16a ()
  (let* ((groups (split-groups (read-file "day16.txt") '() '()))
	 (field-specs (mapcar #'parse-field-spec (car groups)))
	 (tickets (mapcar #'parse-num-list (cdr (third groups)))))
    (apply #'+ (remove-if #'null (mapcar (lambda (ticket) (invalid-values ticket field-specs)) tickets)))))

(defun valid-next-field-specs (field-specs tickets)
  (if (null field-specs) nil
      (if (every (lambda (val) (check-range val (car field-specs)))
		 (mapcar #'car tickets))
	  (cons (car field-specs) (valid-next-field-specs (cdr field-specs) tickets))
	  (valid-next-field-specs (cdr field-specs) tickets))))

(defun try-field-specs (valid-field-specs field-specs tickets)
  (if (null valid-field-specs) nil
      (let* ((field-spec (car valid-field-specs))
	     (other-fields (remove-if (lambda (fs) (equal (car fs) (car field-spec))) field-specs))
	     (rest (determine-field-spec-order other-fields (mapcar #'cdr tickets))))
	(if rest (cons field-spec rest)
	    (try-field-specs (cdr valid-field-specs) field-specs tickets)))))

(defun determine-field-spec-order (field-specs tickets)
  (cond ((and (null (cdr field-specs))
	      (= 0 (length (valid-next-field-specs field-specs tickets))))
	 nil)
	((null (cdr field-specs)) field-specs)
	(t (try-field-specs (valid-next-field-specs field-specs tickets) field-specs tickets))))

(defun is-departure (name)
  (and (>= (length name) 9)
       (equal "departure" (subseq name 0 9))))

(defun day16b ()
  (let* ((groups (split-groups (read-file "day16.txt") '() '()))
	 (field-specs (mapcar #'parse-field-spec (car groups)))
	 (my-ticket (parse-num-list (cadr (cadr groups))))
	 (all-tickets (mapcar #'parse-num-list (cdr (third groups))))
	 (tickets (remove-invalid-values all-tickets field-specs))
	 (field-order (determine-field-spec-order field-specs tickets)))
    (apply #'* (mapcar (lambda (fs ticket-val)
			 (if (is-departure (car fs)) ticket-val 1))
		       field-order my-ticket)
     )))
