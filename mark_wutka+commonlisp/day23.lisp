(load "mwlib.lisp")

;;; This solution is a little less functional in that
;;; it requires some pointer manipulation to implement
;;; a circular list

;;; Compute the destination. We only need to look at the
;;; removed cups when computing the next destination. Since
;;; the list of removed cups is a piece of the circular list
;;; we use (take 3) to just grab the first items into a
;;; non-circular list
(defun destination (removed-cups num max)
  (if (< num 1) (destination removed-cups max max)      
      (if (not (member num (take 3 removed-cups))) num
	  (destination removed-cups (1- num) max))))

;;; Insert the new cups after the given destination number
;;; We create an index of all the cup cons cells at the beginning
;;; and since we are only changing the cdr part of the cells and
;;; never creating new ones, the index always points to the cell
;;; containing the given number (i.e. index[5] = the cell containing 5
;;; and the reference to the cell that comes after 5.
;;; rplacd is a pointer manipulation that changes the cdr of a cons cell
;;; to point to a different place
(defun insert-cups (new-cups dest index)
  (let ((cup-list (aref index dest)))
;;; Make the last cup in the new-cups list point to the cup after the
;;; one we are inserting after
;;; If the cddr seems like it is one short, remember that (rplacd new-cups ...)
;;; would set the next for the first item in new-cups, and (rplacd (cdr new-cups) ...)
;;; would set the next for the second item in new-cups, so (rplacd (cddr new-cups) ...)
;;; must set the next cell for the third
    (rplacd (cddr new-cups) (cdr cup-list))
;;; Make the cup at the insertion point to the new cups list as the next
;;; item in the list
    (rplacd cup-list new-cups)
    cup-list))

(defun play-round (cups max-cup index)
  ;;; Get the reference to the 3 cups to move
  (let* ((next-3 (cdr cups))
	 ;;; Remove the 3 cups from the list by making the cdr part of the
	 ;;; current cons cell point to to the one 4 after this one
	 (new-cup-list (rplacd cups (cddddr cups)))
	 ;;; Figure out the destination index
	 (dest (destination next-3 (1- (car new-cup-list)) max-cup)))
    ;;; Insert the removed cups back at the designated index
    (insert-cups next-3 dest index)
    ;;; Shift over to the next cup
    (cdr new-cup-list)))

;;; Keep playing rounds until the rounds counter hits 0
(defun play-rounds (cups rounds max-cup index)
  (if (< rounds 1) cups
      (play-rounds (play-round cups max-cup index) (1- rounds) max-cup index)))

;;; Creates a circular list from an existing list. We make a copy of the
;;; initial list so it doesn't modify the original list. Basically, we keep track
;;; of the head of the list while moving down to the tail, and when we hit the
;;; cons cell whose cdr is nil, we set that to the head of the list, so the tail
;;; points back to the head. Once you do this, you have to be careful in the repl
;;; so it doesn't try to print the list. You can use the take function in mwlib to
;;; always take some fixed number of items.
(defun make-circular-list (l)
  (labels ((make-circular-rec (l lstart)
	     ;;; If we hit the tail, make the tail point to the head
	     (if (null (cdr l)) (rplacd l lstart)
		 ;;; Otherwise keep looking for the tail
		 (make-circular-rec (cdr l) lstart))))
    ;;; Lstart is a clone of the original list made by mapping the identity function
    (let ((lstart (mapcar #'identity l)))
      ;;; The return value of rplacd, and this make-circular-rec is the cdr that
      ;;; was modified, which was the tail, so we return the cdr of that, which
      ;;; is now the head of the list
      (cdr (make-circular-rec lstart lstart)))))

;;; Create a vector mapping the values in a list to the cons cells containing
;;; each value. It is assumed that there are no gaps in the numbers and that they
;;; range from 1..n, so we make the vector 1 larger than the list length since
;;; index 0 will be unused.
;;; We have to pass in the list size since it is a circular list and the length
;;; function would loop forever. We could make a circular-aware length function that
;;; hangs onto the head of the list when it recurses and stops when it gets back to
;;; the head, but for this program, we know the size each time
(defun make-index (number-list size)
  (let ((index (make-array (1+ size) :initial-element 0)))
    ;;; 
    (labels ((make-index-rec (n count)
	       (when (> count 0)
		 ;;; Use the number in the current cons cell as the array index,
		 ;;; store the cons cell itself in the array
		 (setf (aref index (car n)) n)
		 ;;; Repeat the procedure for the next item in the list
		 (make-index-rec (cdr n) (1- count)))))
      (make-index-rec number-list size)
      index))
  )

(defun day23a ()
  (let* ((start-list '(1 2 3 4 8 7 5 9 6))
	 ;;; Turn it into a circular list
	 (circular-start-list (make-circular-list start-list))
	 ;;; Create an index for the list
	 (index (make-index circular-start-list 9)))
    (play-rounds circular-start-list 100 9 index)
    (format t "~{~D~}~%" (take 8 (cdr (aref index 1)))))
  )

(defun day23b ()
  ;;; Use iota to create the list of numbers up to 1000000, replace the first 9
  ;;; with the puzzle input list
  (let* ((number-list (append '(1 2 3 4 8 7 5 9 6)
			      (drop 9 (iota 1000000 :start 1))))
	 ;;; Turn it into a circular list
	 (circular-number-list (make-circular-list number-list))
	 ;;; Compute an index for the list
	 (index (make-index circular-number-list 1000000)))
    (play-rounds circular-number-list 10000000 1000000 index)
    ;;; (aref index 1) points to the cell containing 1, so we want the
    ;;; item after that cell (second) and the item after that one (third)
    (* (second (aref index 1)) (third (aref index 1)))))

