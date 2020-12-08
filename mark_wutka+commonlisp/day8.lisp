(ql:quickload :cl-ppcre)
(load "mwlib.lisp")

(defun parse-inst (line)
  (ppcre:register-groups-bind (inst (#'parse-integer offset))
			      ("([^ ]*) (.*)" line)
			      (list inst offset)))

(defun parse-lines (lines)
  (map 'vector #'parse-inst lines))

(defun execute-1 (instructions offset accum visited-locs)
  (let* ((loc (aref instructions offset))
	 (inst (car loc))
	 (inst-offset (cadr loc))
	 (visited (find offset visited-locs)))
    (cond (visited accum)
	  ((equal inst "acc") (execute-1 instructions (1+ offset) (+ accum inst-offset) (cons offset visited-locs)))
	  ((equal inst "nop") (execute-1 instructions (1+ offset) accum (cons offset visited-locs)))
	  ((equal inst "jmp") (execute-1 instructions (+ offset inst-offset) accum (cons offset visited-locs))))))

(defun day1a ()
  (let* ((lines (read-file "day8.txt"))
	 (insts (parse-lines lines)))
    (execute-1 insts 0 0 '())))

(defun execute-2 (instructions offset accum visited-locs try-change)
  (if (= offset (length instructions)) accum
      (if (> offset (length instructions)) nil
	  (let* ((loc (aref instructions offset))
		 (inst (car loc))
		 (inst-offset (cadr loc))
		 (visited (find offset visited-locs)))
	    (cond (visited nil)
		  ((equal inst "acc")
		   (execute-2 instructions (1+ offset) (+ accum inst-offset) (cons offset visited-locs) try-change))
		  ((equal inst "nop")
		   ;;; Try nop first, if the result is not nil, return it
		   (let ((res (execute-2 instructions (1+ offset) accum (cons offset visited-locs) try-change)))
		     (if res res
			 ;;; If we are allowed to try changing nop to jmp, try it, otherwise fail this try
			 (if try-change 
			     (execute-2 instructions (+ offset inst-offset) accum (cons offset visited-locs) nil)
			     nil))))
		  ((equal inst "jmp")
		   ;;; Try jmp first, if the result is not nil, return it
		   (let ((res (execute-2 instructions (+ offset inst-offset) accum (cons offset visited-locs) try-change)))
		     (if res res
			 ;;; If we are allowed to try changing jmp to nop, try it, otherwise fail this try
			 (if try-change
			     (execute-2 instructions (1+ offset) accum (cons offset visited-locs) nil)
			     nil)))))))))

(defun day1b ()
  (let* ((lines (read-file "day8.txt"))
	 (insts (parse-lines lines)))
    (execute-2 insts 0 0 '() t)))
