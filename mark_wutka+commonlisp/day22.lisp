(load "mwlib.lisp")

;;; Tail-recursive first round gtame
(defun play-game (deck1 deck2)
  (if (or (= 0 (length deck1))
	  (= 0 (length deck2)))
      ;;; If either deck is length 0, return the decks
      (list deck1 deck2)
      ;;; Compare the first card in each deck
      (if (> (car deck1) (car deck2))
	  ;;; Append the winning and losing cards to player 1's deck
	  (play-game (append (cdr deck1) (list (car deck1) (car deck2)))
		     (cdr deck2))
	  ;;; Append the winning and losing cards to player 2's deck	  
	  (play-game (cdr deck1) (append (cdr deck2) (list (car deck2) (car deck1)))))))

(defun parse-deck (deck)
  (mapcar #'parse-integer (cdr deck)))

(defun compute-score (deck)
  ;;; iota normally returns 0..n-1, so go to length+1 and drop the first to get 1..n
  ;;; We reverse the deck, multiply each item by the iota list starting at 1
  ;;; and add the results
  (apply #'+ (mapcar #'* (reverse deck) (cdr (iota (1+ (length deck)))))))

(defun day22a ()
  (let* ((decks (mapcar #'parse-deck (split-groups (read-file "day22.txt") '() '())))
	 (result (play-game (car decks) (cadr decks))))
    (if (> (length (car result)) 0)
	;;; If player 1 won, score the first deck
	(compute-score (car result))
	;;; Otherwise score the second deck
	(compute-score (cadr result)))))

(defun play-recursive-game (deck1 deck2 previous-decks)
  ;;; If player 1's deck is empty, player 2 wins
  ;;; If player 2's deck is empty, player 1 wins
  (cond ((= (length deck1) 0) (list 2 deck1 deck2))
	((= (length deck2) 0) (list 1 deck1 deck2))
	;;; If this combination of decks has occurred before, player 1 wins
	((member (list deck1 deck2) previous-decks :test #'equalp)
	 (list 1 deck1 deck2))
	;;; If there are enough cards to play a recursive game, play it
	((and (<= (car deck1) (length (cdr deck1)))
	      (<= (car deck2) (length (cdr deck2))))
	 (let ((result (play-recursive-game (take (car deck1) (cdr deck1))
					    (take (car deck2) (cdr deck2)) '())))
	   (if (= 1 (car result))
	       ;;; If player 1 won the recursive game, append both cards to player 1's deck
	       (play-recursive-game (append (cdr deck1) (list (car deck1) (car deck2)))
				    (cdr deck2) (cons (list deck1 deck2) previous-decks))
	       ;;; Otherwise append both cards to player 2's deck
	       (play-recursive-game (cdr deck1) (append (cdr deck2) (list (car deck2) (car deck1)))
				    (cons (list deck1 deck2) previous-decks)))))
	(t (if (> (car deck1) (car deck2))
	       ;;; Same as in the first round, if player 1 had the highest card, append
	       ;;; both cards to player 1's deck
	       (play-recursive-game (append (cdr deck1) (list (car deck1) (car deck2)))
				    (cdr deck2) (cons (list deck1 deck2) previous-decks))
	       ;;; Otherwise append both cards to player 2's deck
	       (play-recursive-game (cdr deck1) (append (cdr deck2) (list (car deck2) (car deck1)))
				    (cons (list deck1 deck2) previous-decks))))))

(defun day22b ()
  (let* ((decks (mapcar #'parse-deck (split-groups (read-file "day22.txt") '() '())))
	 (result (play-recursive-game (car decks) (cadr decks) '())))
    (if (= (car result) 1)
	(compute-score (cadr result))
	(compute-score (caddr result))))
)
