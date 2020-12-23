(load "mwlib.lisp")

;;; After finishing the day23 solution I realized that instead of
;;; keeping a circular list, I could dispense with the circular list
;;; and just keep a vector of next indices, so the number that comes
;;; after 1 in the list is stored at nexts[1]
;;; I also changed destination and insert-cups to not take a list of
;;; new cups, which significantly reduced the object creation and cut
;;; the execution time by more than half.

(defun destination (rc1 rc2 rc3 num max)
  (if (< num 1) (destination rc1 rc2 rc3 max max)      
      (if (not (or (= num rc1) (= num rc2) (= num rc3))) num
	  (destination rc1 rc2 rc3 (1- num) max))))

;;; Insert the new cups after the given destination number
(defun insert-cups (first-new last-new dest nexts)
    ;;; The last cup in the inserted cups should point to the cell
    ;;; that dest currently points to
  (setf (aref nexts last-new) (aref nexts dest))
    ;;; Dest should now point to the first cell in new-cups
  (setf (aref nexts dest) first-new))

(defun play-round (nexts curr-cup max-cup)
  ;;; Get the reference to the 3 cups to move
  (let* ((next-1 (aref nexts curr-cup))
	 (next-2 (aref nexts next-1))
	 (next-3 (aref nexts next-2)))    
;;; Remove the 3 cups from the list by making curr-cup point
 ;;; to what next-3 pointed to
    (setf (aref nexts curr-cup) (aref nexts next-3))

    ;;; Insert the removed cups back at the designated index
    (insert-cups next-1 next-3 (destination next-1 next-2 next-3
					(1- curr-cup)
					max-cup)
		 nexts)
    ;;; Shift over to the next cup
    (aref nexts curr-cup)))

;;; Keep playing rounds until the rounds counter hits 0
(defun play-rounds (nexts curr-cup rounds max-cup)
  (if (< rounds 1) curr-cup
      (play-rounds nexts (play-round nexts curr-cup max-cup) (1- rounds) max-cup)))

;;; Creates a vector of next indexes by mapping over the list and the lest
;;; rotated by one position (put the first element on the end)
(defun make-nexts (number-list)
  (let ((nexts (make-array (1+ (length number-list)))))
    (mapcar (lambda (a b) (setf (aref nexts a) b))
	    number-list (append (cdr number-list) (list (car number-list))))
    nexts))

;;; Returns a chain of num items starting at start
(defun get-chain (start num nexts)
  (if (= num 0) '()
      (cons start (get-chain (aref nexts start) (1- num) nexts))))

(defun day23a ()
  (let* ((start-list '(1 2 3 4 8 7 5 9 6))
	 (nexts (make-nexts start-list)))
    (play-rounds nexts (car start-list) 100 9)
    (format t "~{~D~}~%" (cdr (get-chain 1 9 nexts)))))

(defun day23b ()
  ;;; Use iota to create the list of numbers up to 1000000, replace the first 9
  ;;; with the puzzle input list
  (let* ((number-list (append '(1 2 3 4 8 7 5 9 6)
			      (drop 9 (iota 1000000 :start 1))))
	 (nexts (make-nexts number-list)))
    (play-rounds nexts (car number-list) 10000000 1000000)
    ;;; (aref index 1) points to the cell containing 1, so we want the
    ;;; item after that cell (second) and the item after that one (third)
    (apply #'* (cdr (get-chain 1 3 nexts)))))

