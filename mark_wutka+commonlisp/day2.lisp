;;;You need ppcre loaded
;;;(ql:quickload "cl-ppcre")

(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

;;; Parse line with regex, parse low and high as integers, return 1st char of ch since
;;; the regex parser returns it as a string and we want it as a character
(defun parse-password (line)
  (ppcre:register-groups-bind ((#'parse-integer low high) ch password)
      ("([0-9]*)-([0-9]*) ([a-z]): ([a-z]*)" line)
    (values low high (char ch 0) password)))

;;; In part A, a password is valid if the number of times ch occurs is between
;;; low and high
(defun is-valid-password-a (line)
  (multiple-value-bind (low high ch password)
      (parse-password line)
    (let* ((num-occur (length (remove-if-not (lambda (ch1) (eql ch ch1)) password))))
      (and (>= num-occur low) (<= num-occur high)))))

;;; In part B, a password is valid if ch occurs either at pos1 or pos2 (index starts at 1)
;;; but it can't occur at both
(defun is-valid-password-b (line)
  (multiple-value-bind (pos1 pos2 ch password)
      (parse-password line)
    (let ((ch1 (eql ch (char password (- pos1 1))))
	  (ch2 (eql ch (char password (- pos2 1)))))
      (or (and ch1 (not ch2))
	  (and (not ch1) ch2)))))

(defun day2a ()
  (let ((pwds (read-file "day2.txt")))
    (length (remove-if-not #'is-valid-password-a pwds))))

(defun day2b ()
  (let ((pwds (read-file "day2.txt")))
    (length (remove-if-not #'is-valid-password-b pwds))))
