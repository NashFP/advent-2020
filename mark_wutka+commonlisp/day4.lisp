(ql:quickload :cl-ppcre)

(load "mwlib.lisp")

;;; Define all the valid fields
(defvar valid-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"))

;;; Define the required fields
(defvar required-fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))

;;; Define the valid values for eye color
(defvar valid-eye-colors '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))

;;; Returns non-nil if the field is in the list of valid fields
(defun is-valid-id-field (f)
  (find (car f) valid-fields :test #'equal))

;;; Split the line by space, then split each item by :, filter out invalid fields
(defun parse-id-line (line)
  (remove-if-not #'is-valid-id-field (mapcar (lambda (p) (ppcre:split #\: p))
					   (ppcre:split #\Space line))))

;;; Parse all the lines into a list of ids. It builds up an id adding the fields on each line
;;; and then when it encounters a blank line or the end of the list, it
;;; prepends the current id to the id list.
;;; Because it builds the list by prepending, the first id in the file is the last in the list
(defun parse-lines (lines ids curr-id)
  (cond ((null lines) (if (null curr-id) ids (cons curr-id ids)))
	((= (length (car lines)) 0) (parse-lines (cdr lines) (cons curr-id ids) '()))
	(t (parse-lines (cdr lines)
			ids
			(append (parse-id-line (car lines)) curr-id)))))

;;; Returns true if the id has a field
(defun has-field (id field)
 (assoc field id :test #'equal))

;;; Checks whether an id is valid for the A part of day 4
(defun is-valid-id-a (id)
  (every (lambda (field) (has-field id field)) required-fields))

;;; Checks to see that a number can be parsed and is in the correct range
(defun validate-number (num low high)
  (let ((parsed-number (ignore-errors (parse-integer num :junk-allowed t))))
    (and (not (null parsed-number))
	 (>= parsed-number low)
	 (<= parsed-number high))))

(defun validate-birth-year (value)
  (validate-number value 1920 2002))

(defun validate-issue-year (value)
  (validate-number value 2010 2020))

(defun validate-expiration-year (value)
  (validate-number value 2020 2030))

;;; Checks whether the height ends with cm or in, and that the value
;;; for each is in the correct range
(defun validate-height (value)
  (cond ((< (length value) 3) nil)
	((equal (subseq value (- (length value) 2) (length value)) "cm")
	 (validate-number (subseq value 0 (- (length value) 2)) 150 193))
	((equal (subseq value (- (length value) 2) (length value)) "in")
	 (validate-number (subseq value 0 (- (length value) 2)) 59 76))
	(t nil)))

;;; Checks that the hair color starts with a # and then a 6-digit hex number
(defun validate-hair-color (value)
  (and (= (length value) 7)
       (eql (aref value 0) #\#)
       (not (null (ignore-errors (parse-integer value :start 1 :radix 16))))))

;;; Checks that the eye color is in the list of valid eye colors
(defun validate-eye-color (value)
  (member value valid-eye-colors :test #'equal))

;;; Make sure the passport id is a 9-digit number
(defun validate-passport-id (value)
  (and (= (length value) 9)
       (not (null (ignore-errors (parse-integer value))))))

;;; No validation needed for country id
(defun validate-country-id (value) t)

(defvar validators (list
		    (cons "byr" #'validate-birth-year)
		    (cons "iyr" #'validate-issue-year)
		    (cons "eyr" #'validate-expiration-year)
		    (cons "hgt" #'validate-height)
		    (cons "hcl" #'validate-hair-color)
		    (cons "ecl" #'validate-eye-color)
		    (cons "pid" #'validate-passport-id)
		    (cons "cid" #'validate-country-id)))

;;; Look up the validator for the field in the validators list and run it
(defun is-field-value-valid (field)
  (let ((validator (assoc (car field) validators :test #'equal)))
    (cond ((null validator) nil)
	  ((apply (cdr validator) (list (cadr field)))))))


;;; For each field in the id, make sure it is valid
(defun is-id-field-valid (id field)
  (let ((field-in-id (assoc field id :test #'equal)))
    (cond ((null field-in-id) nil)
	  (t (is-field-value-valid field-in-id)))))

;;; For the B part of Day 4, make sure that the id has each required field and that the
;;; value for that field is valid
(defun is-valid-id-b (id)
  (every (lambda (field) (and (has-field id field) (is-id-field-valid id field))) required-fields))

(defun day4a ()
  (let ((ids (parse-lines (read-file "day4.txt") '() '())))
    (length (remove-if-not #'is-valid-id-a ids))))

(defun day4b ()
  (let ((ids (parse-lines (read-file "day4.txt") '() '())))
    (length (remove-if-not #'is-valid-id-b ids))))
