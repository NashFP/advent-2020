(load "mwlib.lisp")

;;; Tail-recursive first round gtame
(defun play-game (deck1 deck2)
  (if (or (= 0 (length deck1))
	  (= 0 (length deck2)))
      ;;; If either deck is length 0, return the decks
      (list deck1 deck2)
      ;;; Compare the first card in each deck
      (let ((p1-card (car deck1)) (p1-deck (cdr deck1))
	    (p2-card (car deck2)) (p2-deck (cdr deck2)))
	(if (> p1-card p2-card)
	  ;;; Append the winning and losing cards to player 1's deck
	    (play-game (append p1-deck (list p1-card p2-card)) p2-deck)
	  ;;; Append the winning and losing cards to player 2's deck	  
	    (play-game p1-deck (append p2-deck (list p2-card p1-card)))))))

(defun parse-deck (deck)
  (mapcar #'parse-integer (cdr deck)))

(defun compute-score (deck)
  (apply #'+ (mapcar #'* (reverse deck) (iota (length deck) :start 1))))

(defun day22a ()
  (let* ((decks (mapcar #'parse-deck (split-groups (read-file "day22.txt") '() '())))
	 (result (play-game (car decks) (cadr decks))))
    (if (> (length (car result)) 0)
	;;; If player 1 won, score the first deck
	(compute-score (car result))
	;;; Otherwise score the second deck
	(compute-score (cadr result)))))

(defun play-recursive-game (deck1 deck2 previous-decks)
  ;;; The car and cdr functions return nil if you call them with nil
  ;;; so it is safe to do these assignments before checking the list lengths
  (let ((p1-card (car deck1)) (p1-deck (cdr deck1))
	(p2-card (car deck2)) (p2-deck (cdr deck2)))    
  ;;; If player 1's deck is empty, player 2 wins
  ;;; If player 2's deck is empty, player 1 wins
    (cond ((= (length deck1) 0) (list 2 deck1 deck2))
	  ((= (length deck2) 0) (list 1 deck1 deck2))
	;;; If this combination of decks has occurred before, player 1 wins
	  ((member (list deck1 deck2) previous-decks :test #'equalp)
	   (list 1 deck1 deck2))
	;;; If there are enough cards to play a recursive game, play it
	  ((and (<= p1-card (length p1-deck))
		(<= p2-card (length p2-deck)))
	   (let ((result (play-recursive-game (take p1-card p1-deck)
					      (take p2-card p2-deck) '())))
	     (if (= 1 (car result))
	       ;;; If player 1 won the recursive game, append both cards to player 1's deck
		 (play-recursive-game (append p1-deck (list p1-card p2-card))
				      p2-deck (cons (list deck1 deck2) previous-decks))
	       ;;; Otherwise append both cards to player 2's deck
		 (play-recursive-game p1-deck (append p2-deck (list p2-card p1-card))
				      (cons (list deck1 deck2) previous-decks)))))
	  (t (if (> p1-card p2-card)
	       ;;; Same as in the first round, if player 1 had the highest card, append
	       ;;; both cards to player 1's deck
		 (play-recursive-game (append p1-deck (list p1-card p2-card))
				      p2-deck (cons (list deck1 deck2) previous-decks))
	       ;;; Otherwise append both cards to player 2's deck
		 (play-recursive-game p1-deck (append p2-deck (list p2-card p1-card))
				      (cons (list deck1 deck2) previous-decks)))))))

(defun day22b ()
  (let* ((decks (mapcar #'parse-deck (split-groups (read-file "day22.txt") '() '())))
	 (result (play-recursive-game (first decks) (second decks) '())))
    (if (= (first result) 1)
	(compute-score (second result))
	(compute-score (third result))))
)
