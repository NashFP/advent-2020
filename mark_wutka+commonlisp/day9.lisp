
(load "mwlib.lisp")

(defun is-valid-number-1 (n m prelude)
  (some (lambda (p) (= n (+ m p))) prelude))

(defun is-valid-number (n prelude)
  (some (lambda (p) (is-valid-number-1 n p prelude)) prelude))

;;; Tail-recursive search for invalid numbers, building up a list of invalid numbers
(defun find-invalid-numbers-1 (prelude numbers invalids)
  ;;; The list of invalid numbers is built in reverse, so reverse
  ;;; it before returning
  (if (null numbers) (reverse invalids)
      (let* ((n (car numbers))
	     (numbers-rest (cdr numbers))
	     ;;; Keep a running prelude, drop the first number, add n to the end
	     (next-prelude (append (cdr prelude) (list n))))
	(if (not (is-valid-number n prelude))
	    (find-invalid-numbers-1 next-prelude numbers-rest (cons n invalids))
	    (find-invalid-numbers-1 next-prelude numbers-rest invalids)))))

(defun find-invalid-numbers (numbers)
  (let ((prelude (take 25 numbers))
	(num-list (drop 25 numbers)))
    (find-invalid-numbers-1 prelude num-list '())))

(defun day9a ()
  (let ((numbers (mapcar #'parse-integer (read-file "day9.txt"))))
    (car (find-invalid-numbers numbers))))


;;; If, starting at the beginning, some range of numbers adds to n,
;;; return the range. This function is tail-recursive so it builds
;;; the range in reverse and then reverses it before returning
(defun adds-to (n numbers range)
  (cond ((null numbers) nil)
	;;; If n has been reduced to 0 then the numbers added to it exactly
	((= 0 n) (reverse range))
	;;; If n < 0 then the numbers didn't add exactly
	((< n 0) nil)
	;;; Reduce n by the first number in numbers and repeat the search
	(t (adds-to (- n (car numbers)) (cdr numbers) (cons (car numbers) range)))))

;;; Look for the first starting place in numbers where a contiguous list of
;;; numbers adds to target
(defun find-contiguous (target numbers)
  (if (null numbers) nil
      (let ((range (adds-to target numbers '())))
	;;; If range is not null, we found it
	(if range range
	    ;;; Otherwise drop the first number and search again
	    (find-contiguous target (cdr numbers))))))

(defun day9b ()
  (let* ((numbers (mapcar #'parse-integer (read-file "day9.txt")))
	 (first-invalid (car (find-invalid-numbers numbers)))
	 (contig (find-contiguous first-invalid numbers)))
    (+ (apply #'min contig) (apply #'max contig))))
