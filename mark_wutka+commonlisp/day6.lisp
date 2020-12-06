(load "mwlib.lisp")

;;; Need the string as a list for set operations
(defun string->list (str)
  (loop for ch across str collect ch))

;;; Create a set that is a union of all the characters in each string
(defun group-to-set (group)
  (reduce #'union (mapcar #'string->list group)))

;;; Create a set that is the intersection of all the characters in the string
(defun group-to-intersection-set (group)
  (reduce #'intersection (mapcar #'string->list group)))

;;; Convert each group of answers to a set by taking the union of the answers,
;;; take the length, and sum the lengths
;;; Note: I moved split-groups into mwlib.lisp since there have already been
;;; two puzzles that have grouped things by blank lines
(defun day6a ()
  (let ((lines (read-file "day6.txt")))
    (apply #'+ (mapcar #'length
		       (mapcar #'group-to-set (split-groups lines '() '()))))))

;;; Convert each group of answers to a set by taking the intersection of the answers,
;;; take the lenght, and sum the lengths
(defun day6b ()
  (let ((lines (read-file "day6.txt")))
    (apply #'+ (mapcar #'length
		       (mapcar #'group-to-intersection-set (split-groups lines '() '()))))))
