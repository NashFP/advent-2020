
;;; parse-contains1 takes a list of the strings after "contain" in a list, taking the count,
;;; and the two bag part names. If a bag contains no other bags, it must check for n
;;; being "no". This function is tail-recursive, the cddddr drops the first 4 elements of
;;; the list, since each contained bag is described with 4 strings.
(defun parse-contains1 (contains contains-list)
  (cond ((null contains) contains-list)
	(t (let ((n (first contains))
		 (c1 (second contains))
		 (c2 (third contains)))
	     (if (equal n "no")
		 '()
		 (parse-contains1 (cddddr contains) (cons (list (concatenate 'string c1 " " c2) (parse-integer n)) contains-list))
		 )))))

;;; parse-contains parses the part of the string after "contain" by splitting on space, and passing
;;; it off to the recursive parse-contains1 function
(defun parse-contains (contains)
  (let ((s (ppcre:split "\\s+" contains)))
    (parse-contains1 s '())))

;;; parse-line splits out the bag type and the list of contained bags using a regex
(defun parse-line (line)
  (ppcre:register-groups-bind (bag-type contains)
			      ("([^ ]* [^ ]*) bags contain (.*)" line)
    (list bag-type (parse-contains contains))))

;;; build-bag-table creates a hash table of bags where the key is the bag name
;;; and the value is a list where each element of the list is a list containing
;;; the contained bag name and how many of that bag are contained
(defun build-bag-table (lines)
  (let ((bag-table (make-hash-table :test #'equal)))
    (dolist (line lines)
      (let* ((parsed (parse-line line))
	     (bag-type (car parsed))
	     (contains (cadr parsed)))
	(setf (gethash bag-type bag-table) contains)))
    bag-table))

;;; contains-bag returns true if source-bag can contain target-bag somewhere down
;;; in the hierarchy. We use assoc to see if it directly contains the bag, and
;;; if not, recursively call contains-bag on each contained bag, using the some
;;; function to find at least one
(defun contains-bag (target-bag source-bag bag-table)
  (let ((source (gethash source-bag bag-table)))
    (if (assoc target-bag source :test #'equal)
	t
	(some (lambda (src) (contains-bag target-bag src bag-table))
	      (mapcar #'car source)))))

;;; count-container-bags counts the number of bags that can contain target-bag
(defun count-container-bags (target-bag bag-table)
  (loop for key being the hash-keys of bag-table
     if (contains-bag target-bag key bag-table)
     count 1 into found
     finally (return found)))

(defun day1a ()
  (let* ((lines (read-file "day7.txt"))
	 (bag-table (build-bag-table lines)))
    (count-container-bags "shiny gold" bag-table)))

;;; count-contained-bags counts the number of bags that a bag can contain.
;;; For each bag that target-bag contains, we find its count-contained-bags count, and add
;;; 1 to it for the bag itself, and then multiply that by the number of times that target-bag
;;; contains that bag. We then sum those numbers and that's the total bags this bag can contain
(defun count-contained-bags (target-bag bag-table)
       (let ((contained-bags (gethash target-bag bag-table)))
	 (apply #'+
		(mapcar
		 ;;; c is an entry in the list of contained bags and looks like ("some bag" 123)
		 ;;; so (car c) is the bag name and (cadr c) is the count of that bag
		 (lambda (c) (* (1+ (count-contained-bags (car c) bag-table)) (cadr c)))
		 contained-bags))))

(defun day1b ()
  (let* ((lines (read-file "day7.txt"))
	 (bag-table (build-bag-table lines)))
    (count-contained-bags "shiny gold" bag-table)))
