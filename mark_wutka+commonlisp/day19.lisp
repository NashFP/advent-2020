
(load "mwlib.lisp")
(ql:quickload :cl-ppcre)

;;; If a rule item is a string like "a", remove the quotes
;;; otherwise parse it as an integer
(defun parse-rule-item (item)
  (if (char= #\" (char item 0)) (remove #\" item)
      (parse-integer item)))

;;; Split the rules by | and parse each set separately,
;;; returning a list of lists
(defun parse-rule (rule)
  (let ((options (ppcre:split " *\\| *" rule)))
    (mapcar (lambda (opt)
	      (mapcar #'parse-rule-item (ppcre:split " " opt)))
	    options)))

;;; Separate the rule number from the rules and then parse the rules
;;; Return the result as a list (rule-number ((rule-list-1) (rule-list-2)))
(defun parse-line (line)
  (ppcre:register-groups-bind ((#'parse-integer rule-number) rule)
      ("([0-9]*): (.*)" line)
    (list rule-number (parse-rule rule))))

;;; Create a vector from the list of rules, using the rule number as the
;;; index into the vector. This will fail if there are gaps in the rule
;;; numbers
(defun make-rule-set (parsed-lines)
  (let ((rules (make-array (length parsed-lines) :initial-element '())))
    (mapcar (lambda (l) (setf (aref rules (car l)) (cadr l))) parsed-lines)
    rules))

;;; Try to match a single item from a rule, either a string, or another
;;; rule number. This method returns a list of all the possible new
;;; positions to be in after matching this item
(defun match-rule-item (item rules str pos)
  (if (stringp item)
      ;;; If are are past the end of the string, the match fails
      (if (>= pos (length str)) nil
	  (if (char= (char item 0) (char str pos))
	      ;;; If the next character in the string matches what we want, return
	      ;;; the current position as a matching position
	      (list (1+ pos))
	      '()))
      ;;; Return a list of all the new positions possible by matching this rule number
      (match-rule (aref rules item) rules str pos)))

;;; For each item in the list, get a list of possible new positions after matching each
;;; item, then try matching the next items starting at each possible position
(defun match-rule-items (items rules str pos-list)
  (if (null items) pos-list
      (let ((matched-poses (mapcan (lambda (p) (match-rule-item (car items) rules str p))
				   pos-list)))
	(match-rule-items (cdr items) rules str matched-poses))))

;;; Returns a list of all the possible positions after matching this rule
(defun match-rule (item-lists rules str pos)
  (mapcan (lambda (il) (match-rule-items il rules str (list pos))) item-lists))

;;; Get a list of all the possible positions after matching rule 0, see if
;;; there is at least one position that is equal to the length of the string
(defun matches (rules line)  
  (let ((all-matches (match-rule (aref rules 0) rules line 0)))
    (some (lambda (p) (= p (length line))) all-matches)))

(defun day19a ()
  (let* ((parts (split-groups (read-file "day19.txt") '() '()))
	 (parsed-lines (mapcar #'parse-line (first parts)))
	 (rules (make-rule-set parsed-lines)))
    (length (remove-if-not (lambda (l) (matches rules l)) (second parts)))))

;;; Modify rules 8 and 11
(defun modify-rule (rule)
  (let ((rule-num (car rule)))
    (cond ((= rule-num 8) '(8 ((42) (42 8))))
	  ((= rule-num 11) '(11 ((42 31) (42 11 31))))
	  (t rule))))

(defun day19b ()
  (let* ((parts (split-groups (read-file "day19.txt") '() '()))
	 (parsed-lines (mapcar #'parse-line (first parts)))
	 (rules (make-rule-set (mapcar #'modify-rule parsed-lines))))
    (length (remove-if-not (lambda (l) (matches rules l)) (second parts)))))
