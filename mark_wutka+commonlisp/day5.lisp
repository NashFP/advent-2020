(load "mwlib.lisp")

;;; The seat number encoding scheme is just binary where B and R represent 1 and
;;; F and L represent 0. to-binary converts this to a binary string
(defun to-binary (s)
  (substitute #\1 #\B (substitute #\0 #\F (substitute #\1 #\R (substitute #\0 #\L s)))))

;;; Parse the seat number as a representation of a binary number
(defun parse-bin (s)
  (parse-integer (to-binary s) :radix 2))

;;; Part A is just finding the max of the list
(defun day5a ()
  (let ((lines (read-file "day5.txt")))
    (apply #'max (mapcar #'parse-bin lines))))

;;; Search for a place in a sorted list where the next number is not the previous +1
;;; Since the list is supposed to have one, I skipped checking for null
(defun find-gap (l)
  (if (not (= (cadr l) (1+ (car l))))
      (1+ (car l))
      (find-gap (cdr l))))

;;; Parse the numbers, sort them, and find the gap
(defun day5b ()
  (let ((lines (read-file "day5.txt")))
    (find-gap (sort (mapcar #'parse-bin lines) #'<))))
