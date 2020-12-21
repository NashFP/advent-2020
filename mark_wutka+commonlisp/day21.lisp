(ql:quickload :cl-ppcre)
(load "mwlib.lisp")

(defun parse-line (line)
  (ppcre:register-groups-bind (ingredients allergens)
      ("(.*) \\(contains ([^)]*)\\)" line)
    (list (ppcre:split " +" ingredients)
	  (ppcre:split ", +" allergens))))

(defun get-all-allergens (ingredient-lists)
  (reduce (lambda (a b) (union a b :test #'equal)) (mapcar #'cadr ingredient-lists)))

(defun get-possible-ingredients (ingredient-lists)
  (reduce (lambda (a b) (intersection a b :test #'equal)) (mapcar #'car ingredient-lists)))

(defun has-allergen (allergen ingredient-list)
  (member allergen (cadr ingredient-list) :test #'equal))

(defun get-possible-ingredients-with-allergen (allergen ingredient-lists)
  (get-possible-ingredients (remove-if-not (lambda (il) (has-allergen allergen il))
					   ingredient-lists)))

(defun try-allergen (ingredients-per-allergen)
  (if (= (length ingredients-per-allergen) 1)
      (if (= (length (car ingredients-per-allergen)) 2) ingredients-per-allergen nil)
      (let ((allergen (caar ingredients-per-allergen))
	    (ingredients (cdar ingredients-per-allergen)))
	(labels ((try-ing (ing)
		   (if (null ing) nil
		       (let ((result (try-allergen
				      (mapcar (lambda (il) (remove (car ing) il :test #'equal))
							   (cdr ingredients-per-allergen)))))
			 (if result (cons (cons allergen (list (car ing))) result)
			     (try-ing (cdr ing)))))))
	  (try-ing ingredients)))))

(defun day21a ()
  (let* ((ingredient-lists (mapcar #'parse-line (read-file "day21.txt")))
	 (allergens (get-all-allergens ingredient-lists))
	 (ingredients-per-allergen
	  (mapcar (lambda (i a) (cons a i))
		  (mapcar (lambda (a) (get-possible-ingredients-with-allergen a ingredient-lists))
			  allergens)
		  allergens))
	 (allergen-assignments (try-allergen ingredients-per-allergen))
	 (allergen-ingredients (mapcar #'cadr allergen-assignments)))
    (apply #'+ (mapcar (lambda (il) (length (set-difference (car il) allergen-ingredients)))
		       ingredient-lists))

))
