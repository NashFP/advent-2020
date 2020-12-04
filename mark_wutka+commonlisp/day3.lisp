(load "mwlib.lisp")

;;; Once again the not-so-functional loop macro comes in handy
;;; line steps through each line in treemap, while i is incremented for each line
;;; the position we want in the line is just i*3 mod (length line)
(defun day3a ()
  (let ((treemap (read-file "day3.txt")))
    (loop
       for line in treemap
       for i from 0
	 
       if (eql (aref line (mod (* i 3) (length line))) #\#)
       count 1 into trees
       finally (return trees))))

(defun count-trees (treemap right down)
  (loop
     for line in treemap
     for i from 0

;;; Some tricky math here, we are still iterating each line, and i is the current line number
;;; The modulo computes the current position in the string. We divide i by down to account
;;; for skipping lines, so that we don't move right every time i is incremented, but only
;;; every down times       
     if (and (eql 0 (mod i down)) 
             (eql (aref line (mod (* (/ i down) right) (length line))) #\#))
     count 1 into trees

     finally (return trees)))

(defun day3a-refactored ()
  (let ((treemap (read-file "day3.txt")))
    (count-trees treemap 3 1)))

;;; When you provide multiple lists mapcar it calls the function with multiple values
;;; The first list in mapcar is the number of steps right, the second list is the steps down
;;;
;;; The #'* in the apply is just the multiply function. Common Lisp has 2 namespaces, one for
;;; and one for functions. The #' is how you signal to Lisp that you mean something from the
;;; function namespace instead of the data namespace.
;;; Common Lisp is referred to as a Lisp-2 because of the two namespaces, while Scheme is a Lisp-1
;;;
(defun day3b ()
  (let ((treemap (read-file "day3.txt")))
    (apply #'*
	   (mapcar (lambda (right down) (count-trees treemap right down))
		   '(1 3 5 7 1) '(1 1 1 1 2)))))
