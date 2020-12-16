
(ql:quickload :cl-ppcre)
(ql:quickload :cl-match)
(load "mwlib.lisp")

;;; This version has an optimized implementation of day16b that runs in a fraction of a second,
;;; as opposed to my initial implementation that took minutes.

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


(defun is-departure (name)
  (and (>= (length name) 9)
       (equal "departure" (subseq name 0 9))))

;;; Returns true if every value in column n in the ticket is valid for a field spec
(defun is-valid-column (field-spec n tickets)
  (every (lambda (val) (check-range val field-spec))
	 (map 'list (lambda (ticket-vec) (aref ticket-vec n)) tickets)))

;;; Returns a list of all the columns that are valid for a field spec
(defun valid-columns (field-spec tickets)
  (remove-if-not (lambda (i) (is-valid-column field-spec i tickets)) (iota (length (car tickets)))))

;;; Make a list of field-spec . columns for each column, and then sort the list
;;; so that the field-spec with the fewest possible columns is first
(defun sort-field-order (field-specs tickets)
  (let* ((columns (mapcar (lambda (fs) (valid-columns fs tickets)) field-specs))
	(fields-and-columns (mapcar #'cons field-specs columns)))
    (sort fields-and-columns (lambda (a b) (< (length (cdr a)) (length (cdr b)))))))

;;; The puzzle must be deterministic since there must be a unique answer. It turns out that if
;;; you analyze the possible columns, one field spec has 1 possible column, another has 2, and
;;; so on up to 20. We just take the first one here, and remove its column from the second one's
;;; list, leave it with one, and so on, until we have reduced it to each unique column
(defun reduce-field-order (fields-and-counts prev)
  (if (null fields-and-counts) nil
      (let* ((f-c (car fields-and-counts))
	     (fs (car f-c))
	     (counts (cdr f-c))
	     (curr-count (car (set-difference counts prev)))
	     (next-count (cons curr-count prev)))
	(cons (cons fs curr-count) (reduce-field-order (cdr fields-and-counts) next-count)))))

(defun day16b ()
  (let* ((groups (split-groups (read-file "day16.txt") '() '()))
	 (field-specs (mapcar #'parse-field-spec (car groups)))
	 (my-ticket (parse-num-list (cadr (cadr groups))))
	 (all-tickets (mapcar #'parse-num-list (cdr (third groups))))
	 (tickets (remove-invalid-values all-tickets field-specs))
	 ;;; Since we are now operating over columns, turn the tickets into a list of vectors
	 (ticket-vecs (mapcar (lambda (l) (map 'vector #'identity l)) tickets))
	 (field-order (reduce-field-order (sort-field-order field-specs ticket-vecs) '())))
    (apply #'* (mapcar (lambda (fs)
			 ;;; my-ticket is still a list so use nth to index it instead of aref
			 (if (is-departure (caar fs)) (nth (cdr fs) my-ticket) 1))
		       field-order)
     )))
