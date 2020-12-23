(ql:quickload :cl-ppcre)
(load "mwlib.lisp")

;;; Separate the ingredients from the allergins, split ingredients on space
;;; and allergens on comma-space
(defun parse-line (line)
  (ppcre:register-groups-bind (ingredients allergens)
      ("(.*) \\(contains ([^)]*)\\)" line)
    (list (ppcre:split " +" ingredients)
	  (ppcre:split ", +" allergens))))

;;; Get a list of all the possible allergens
(defun get-all-allergens (ingredient-lists)
  (reduce (lambda (a b) (union a b :test #'equal)) (mapcar #'cadr ingredient-lists)))

;;; Get all the ingredients that occur in each of the ingredient lists
(defun get-possible-ingredients (ingredient-lists)
  (reduce (lambda (a b) (intersection a b :test #'equal)) (mapcar #'car ingredient-lists)))

;;; Returns true if an ingredient list contains an allergen
(defun has-allergen (allergen ingredient-list)
  (member allergen (cadr ingredient-list) :test #'equal))

;;; Get all the ingredients that occur every time a certain allergen is listed
(defun get-possible-ingredients-with-allergen (allergen ingredient-lists)
  (get-possible-ingredients (remove-if-not (lambda (il) (has-allergen allergen il))
					   ingredient-lists)))

;;; Given the ingredients per allergen, try each ingredient as having that allergen
;;; and see if the rest of the assignments can also be completed
(defun try-allergen (ingredients-per-allergen)
  ;;; If there is exactly one ingredient assignment left...
  (if (= (length ingredients-per-allergen) 1)
      ;;; If the assignment has exactly 1 ingredient (length 2 because the allergen
      ;;; is at the beginning of the list, then return this assignment, otherwise
      ;;; return nil indicating there is not an unambiguous solution
      (if (= (length (car ingredients-per-allergen)) 2) ingredients-per-allergen nil)

      ;;; If there are multiple ingredient assignments to try, try each ingredient
      ;;; for this assignment
      (let ((allergen (caar ingredients-per-allergen))
	    (ingredients (cdar ingredients-per-allergen)))
	(labels ((try-ing (ing)
		   ;;; If there are no more ingredients left to try, this solution fails
		   (if (null ing) nil
		       ;;; Otherwise, try the rest of the assignments, removing this ingredient
		       ;;; from the possible ingredients for the rest of the assignments
		       (let ((result (try-allergen
				      (mapcar (lambda (il) (remove (car ing) il :test #'equal))
					      (cdr ingredients-per-allergen)))))
			 ;;; If the result is non-null, the rest of the assignments succeeded,
			 ;;; Put this allergen-ingredient list on the front of the result and
			 ;;; return it
			 (if result (cons (cons allergen (list (car ing))) result)
			     ;;; Otherwise try the next ingredient
			     (try-ing (cdr ing)))))))
	  ;;; Try all the possible ingredients for this allergen
	  (try-ing ingredients)))))

(defun day21a ()
  (let* ((ingredient-lists (mapcar #'parse-line (read-file "day21.txt")))
	 ;;; Get all the possible allergens
	 (allergens (get-all-allergens ingredient-lists))

	 ;;; Get a list of possible ingredients for each allergen
	 (ingredients-per-allergen
	  (mapcar (lambda (i a) (cons a i))
		  (mapcar (lambda (a) (get-possible-ingredients-with-allergen a ingredient-lists))
			  allergens)
		  allergens))
	 ;;; Try to make an unambigious assignment of allergens
	 (allergen-assignments (try-allergen ingredients-per-allergen))
	 ;;; Get the list of ingredients from the assignments
	 (allergen-ingredients (mapcar #'cadr allergen-assignments)))
    ;;; Remove the assigned ingredients from the ingredients in each ingredient list
    ;;; and count not many remain
    (apply #'+ (mapcar (lambda (il)
			 (length (set-difference (car il) allergen-ingredients :test #'equal)))
		       ingredient-lists))

    ))

;;; Sort the ingredient assignments by the name of the allergen
(defun sort-assignments (assignments)
  (sort assignments (lambda (a b) (string< (car a) (car b)))))

(defun day21b ()
  (let* ((ingredient-lists (mapcar #'parse-line (read-file "day21.txt")))
	 ;;; Get all the possible allergens
	 (allergens (get-all-allergens ingredient-lists))

	 ;;; Get a list of possible ingredients for each allergen
	 (ingredients-per-allergen
	  (mapcar (lambda (i a) (cons a i))
		  (mapcar (lambda (a) (get-possible-ingredients-with-allergen a ingredient-lists))
			  allergens)
		  allergens))
	 ;;; Try to make an unambigious assignment of allergens
	 (allergen-assignments (try-allergen ingredients-per-allergen))
	 ;;; Sort the assignments by allergen name
	 (sorted-assignments (sort-assignments allergen-assignments)))
    ;;; Print the list of allergens
    (format t "~{~A,~}" (mapcar #'cadr sorted-assignments))
    ))
