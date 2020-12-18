(load "mwlib.lisp")

(defun do-op (expr)
  ;;; destructuring-bind is for simple pattern-matching
  (destructuring-bind (a op b) expr
    (case op
      (#\+ (+ a b))
      (#\* (* a b)))))

(defun char-to-num (ch)
  (- (char-int ch) (char-int #\0)))

(defun eval-expr (expr reduce-func)
  ;;; labels is like let, but for functions, this is a common pattern in making
  ;;; a nested tail recursive function
  (labels
      ((eval-rec (pos stack )
	 (if (>= pos (length expr))
	     ;;; If we are at the end of the string, reduce the stack and return value and last pos
	     ;;; We reverse the stack because we reduce from left to right
	     (values (car (funcall reduce-func (reverse stack))) pos)

	     ;;; Otherwise, look at the next char
	     (let ((ch (char expr pos)))
	       (cond
		 ;;; Skip space
		 ((eq ch #\Space) (eval-rec (1+ pos) stack))

		 ;;; If the next is a (, recursively evaluate the contained expr
		 ;;; The recursive call will return a value and the next position
		 ;;; after the sub-expression, so push that new value on the stack
		 ;;; and continue evaluating from the next position
		 ((eq ch #\()
		  (multiple-value-bind (new-pos new-stack)
		      ;;; Evaluate the sub expr with a new stack
		      (eval-rec (1+ pos) '())
		    ;;; Continue processing starting at new-pos, pushing the reduced
		    ;;; sub-expression onto the front of the stack
		    (eval-rec new-pos
			      (cons (car (funcall reduce-func (reverse new-stack)))
				    stack))))
		 ;;; If the next is a ), return the next position and the current stack
		 ((eq ch #\)) (values (1+ pos) stack))

		 ;;; If the char is an operator, push it on the stack and keep going
		 ((or (eq ch #\*) (eq ch #\+)) (eval-rec (1+ pos) (cons ch stack)))

		 ;;; If the char is a digit, convert it to an int, push it, and keep going
		 ((digit-char-p ch) (eval-rec (1+ pos) (cons (char-to-num ch) stack)))
		 (t (format t "Unknown char ~A~%" ch)
		    (eval-rec (1+ pos) stack))))))
       )
    ;;; call tail-recursive version with 0 for the starting position and an empty stack
    (eval-rec 0 '())))



;;; Only reduce a set of operators in expressions, leave others alone
(defun reduce-ops (stack ops)
  (let ((parts (take 3 stack)))
    (if (< (length parts) 3) stack
	;;; Is the operator part of the expression in the ops we are processing?
	(if (member (second parts) ops)
	    ;;; Invoke do-op on these 3 items in the stack, push that on the front
	    ;;; and reduce again
	    (reduce-ops (cons (do-op parts)
			      (drop 3 stack))
			ops)
	    ;;; Otherwise, hang onto the first 2 parts (1st arg and operator)
	    ;;; and reduce the rest, then put it back together
	    ;;; So that if + isn't in ops, 1 + 2 * 3 + 4 would hold onto (1 +), evaluate
	    ;;; 2 * 3 + 4, which would eventually return 6 + 4, and the final
	    ;;; reduced expr would be 1 + 6 + 4
	    ;;; (reduce-ops '(1 #\+ 2 #\* 3 #\+ 4) '(#\*))
	    ;;; returns (1 #\+ 6 #\+ 4)
	    (append (take 2 stack) (reduce-ops (drop 2 stack) ops))))))

;;; For the first round, reduce all the operators at the same time
;;; because they have equal precedence
(defun reduce-1 (stack) (reduce-ops stack '(#\+ #\*)))

(defun day18a ()
  (let ((lines (read-file "day18.txt")))
    (apply #'+ (mapcar (lambda (l) (eval-expr l #'reduce-1)) lines))))

;;; For the second round, reduce + first, since it has a higher priority
;;; then reduce *
(defun reduce-2 (stack)
  (reduce-ops (reduce-ops stack '(#\+)) '(#\*)))

(defun day18b ()
  (let ((lines (read-file "day18.txt")))
    (apply #'+ (mapcar (lambda (l) (eval-expr l #'reduce-2)) lines))))
