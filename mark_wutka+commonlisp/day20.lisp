(load "mwlib.lisp")

(defun parse-tile-num (line)
  (parse-integer (subseq line 5 9)))

(defun parse-tile (lines)
  (list (parse-tile-num (car lines)) (cdr lines)))

;;; Each edge has 2 signatures, one a reverse of the other
(defun edge-signatures (tile)
  (let* ((tile-data (cadr tile))
	 (left-edge (map 'string (lambda (s) (char s 0)) tile-data))
	 (right-edge (map 'string (lambda (s) (char s 9)) tile-data))
	 (bottom-edge (car (reverse tile-data))))
    (list (car tile-data) (reverse (car tile-data))
	  left-edge (reverse left-edge)
	  right-edge (reverse right-edge)
	  bottom-edge (reverse bottom-edge))))

;;; Updates the count of occurrences in the signature map
(defun add-sig-to-sigmap (sig sigmap)
  (let ((curr-count (gethash sig sigmap)))
    (if curr-count (setf (gethash sig sigmap) (1+ curr-count))
	(setf (gethash sig sigmap) 1))))

;;; Adds the edge signatures to the map of counts of each signature
(defun add-tile-signatures (tile sigmap)
  (let ((signatures (edge-signatures tile)))
    (mapcar (lambda (s) (add-sig-to-sigmap s sigmap)) signatures)))

;;; Creates a map containing a occurrence count for each edge signature
(defun make-signature-map (tiles)
  (let ((signature-map (make-hash-table :test #'equal)))
    (mapcar (lambda (tile) (add-tile-signatures tile signature-map)) tiles)
    signature-map))

;;; Finds all the signatures in the signature map that occur once
(defun find-unique-signatures (sigmap)
  (loop for k being the hash-keys of sigmap
     when (= 1 (gethash k sigmap))
       collect k))

;;; Counts the number of unique signatures a tile has
(defun num-tile-unique-signatures (tile sigmap)
  (apply #'+ (mapcar (lambda (sig) (if (= 1 (gethash sig sigmap)) 1 0)) (edge-signatures tile))))

;;; Each corner will have 4 unique signatures, two for each corner edge, and two that are
;;; the reverse of the corner edge
(defun get-corners (tiles sigmap)
  (remove-if-not (lambda (tile) (= 4 (num-tile-unique-signatures tile sigmap))) tiles))

(defun day20a ()
  (let* ((tiles (mapcar #'parse-tile (split-groups (read-file "day20.txt") '() '())))
	 (sigmap (make-signature-map tiles))
	 (corners (get-corners tiles sigmap)))
    ;;; Each tile is list containing the tile number and the tile contents
    ;;; Multiply the tile numbers for the corner tiles
    (apply #'* (mapcar #'car corners))))

;;; Adds a tile to the edge map, which maps each possible edge to a list of tiles
(defun add-tile-to-edge-map (tile edge-map)
  (let ((edges (edge-signatures tile)))
    (dolist (edge edges)
      (setf (gethash edge edge-map) (cons tile (gethash edge edge-map))))))

;;; Create a map from edge to a list of tiles
(defun make-edge-map (tiles)
  (let ((edge-map (make-hash-table :test #'equal)))
    (dolist (tile tiles)
      (add-tile-to-edge-map tile edge-map))
    edge-map))

;;; Flip a tile vertically
(defun flip-vertical (tile)
  (list (car tile) (reverse (cadr tile))))

;;; Flip a tile horizontally
(defun flip-horizontal (tile)
  (list (car tile) (mapcar #'reverse (cadr tile))))

;;; Rotate a tile to the right
(defun rotate-right (tile)
  (let* ((tile-data (cadr tile))
	 (tile-size (length tile-data)))
    (list (car tile)
	  (mapcar (lambda (i) (map 'string (lambda (r) (char r i))
				   (reverse tile-data)))
		  (iota tile-size)))))

;;; Create a list of 8 possible tile permutations
(defun permute-tile (tile)
  (let* ((hor-flip (flip-horizontal tile))
	 (vert-flip (flip-vertical tile))
	 (both-flip (flip-vertical hor-flip))
	 (flips (list tile hor-flip vert-flip both-flip)))
    (append flips (mapcar #'rotate-right flips))))

(defun print-tile (tile)
  (dolist (line (cadr tile))
    (format t "~S~%" line)))

(defun print-tile-data (tile-data)
  (dolist (line tile-data)
    (format t "~S~%" line)))

;;; Returns the top edge of a tile
(defun top-edge (tile)
  (caadr tile))

;;; Returns the bottom edge of a tile
(defun bottom-edge (tile)
  (car (reverse (cadr tile))))

;;; Returns the left edge of a tile
(defun left-edge (tile)
  (map 'string (lambda (s) (char s 0)) (cadr tile)))

;;; Returns the right edge of a tile
(defun right-edge (tile)
  (map 'string (lambda (s) (char s 9)) (cadr tile)))

;;; Returns true if this edge exists in only one tile
(defun is-unique (edge edge-map)
  (= 1 (length (gethash edge edge-map))))

;;; Find the permutation of a corner where the top and left
;;; edges are unique
(defun align-corner (corner edge-map)
  (find-if (lambda (p) (and (is-unique (left-edge p) edge-map)
			    (is-unique (top-edge p) edge-map)))
	   (permute-tile corner)))

;;; Finds a tile whose edge computed by dest-edge-func equals
;;; the target edge
(defun find-link (tile target-edge dest-edge-func edge-map)
  (let ((matches (gethash target-edge edge-map)))
    (find-if (lambda (p) (equal target-edge (funcall dest-edge-func p)))
	     (permute-tile (find-if (lambda (other-tile) (not (= (car tile) (car other-tile))))
				    matches)))))

;;; Create a chain of tiles by looking for the file where (source-edge-func tile)
;;; equals (dest-edge-func the-next-tile)
(defun make-chain (tile source-edge-func dest-edge-func edge-map)
  (labels ((make-chain-rec (tile accum)
	     (let ((target-edge (funcall source-edge-func tile)))	       
	       (if (is-unique target-edge edge-map) (reverse (cons tile accum))
		   (make-chain-rec (find-link tile target-edge dest-edge-func edge-map)
				   (cons tile accum))))))
    (make-chain-rec tile '())))

;;; Fit all the tiles together by first finding a corner and aligning it properly
;;; The creating the left edge by chaining from bottom-edge to top-edge
;;; Then create each row by chaining from right-edge to left-edge
(defun fit-tiles (tiles edge-map)
  (let* ((corner (align-corner (car (get-corners tiles (make-signature-map tiles)))
			       edge-map))
	 (left-edge (make-chain corner #'bottom-edge #'top-edge edge-map)))
    (mapcar (lambda (tile) (make-chain tile #'right-edge #'left-edge edge-map))
	    left-edge)))

;;; Strips the borders from a tile
(defun strip-borders (tile)
  (let* ((tile-data (cadr tile))
	 (tile-rows (take (- (length tile-data) 2) (cdr tile-data))))
    (mapcar (lambda (r) (subseq r 1 (1- (length r)))) tile-rows)))

;;; Contatenate all the tiles in a row
(defun merge-tile-rows (row)
  (if (null (car row)) '()
      (cons (apply #'concatenate 'string (mapcar #'car row))
	    (merge-tile-rows (mapcar #'cdr row)))))

;;; Create a big tile from the grid of small ones
(defun make-big-tile (rows)
  (mapcan (lambda (r) (merge-tile-rows (mapcar #'strip-borders r))) rows))

;;; The coordinates of the sea monster pattern
(defvar sea-monster '((18 0)
		     (0 1) (5 1) (6 1) (11 1) (12 1) (17 1) (18 1) (19 1)
		     (1 2) (4 2) (7 2) (10 2) (13 2) (16 2)))

;;; See if this row and column is the beginning of a sea monster pattern
(defun is-sea-monster-present (row col tile-vec)
  (every (lambda (off) (char= #\# (char (aref tile-vec (+ row (cadr off)))
					(+ col (car off)))))
	 sea-monster))

;;; Count the number of sea monsters in a tile
(defun count-sea-monsters (tile)
  (let* ((tile-vec (map 'vector #'identity (cadr tile)))
	 (vec-dim (length tile-vec)))
    ;;; Make a tail-recursive function to iterate over column and row
    (labels ((count-sea-rec (row col sum)
	       ;;; If we are past the last row that could contain a
	       ;;; sea monster, return the count
	       (if (>= row (- vec-dim 2)) sum

		   ;;; If we are past the last column that could contain
		   ;;; a sea monster, go to the next row
		   (if (>= col (- vec-dim 19)) (count-sea-rec (1+ row) 0 sum)
		       (if (is-sea-monster-present row col tile-vec)
			   (count-sea-rec row (1+ col) (1+ sum))
			   (count-sea-rec row (1+ col) sum))))))
      (count-sea-rec 0 0 0))))

;;; Counts the number of pounds in a tile
(defun count-pound (tile)
  (apply #'+ (mapcar (lambda (r) (length (remove #\. r))) (cadr tile))))

(defun day20b ()
  (let* ((tiles (mapcar #'parse-tile (split-groups (read-file "day20.txt") '() '())))	 
	 (edge-map (make-edge-map tiles))
	 (big-tile (make-big-tile (fit-tiles tiles edge-map)))
	 ;;; Find all the permutations of the map
	 (map-permutations (permute-tile (list 0 big-tile)))
	 ;;; Find the permuted tile whose count of sea monsters is > 0
	 (sea-monster-tile (find-if (lambda (tile) (> (count-sea-monsters tile) 0)) map-permutations))
	 ;;; Count the sea monsters again for that tile
	 (sea-monster-count (count-sea-monsters sea-monster-tile)))
    ;;; Compute the non-sea-monster # cells by just counting all the # cells, and
    ;;; subtracting the sea monster count times the length of a sea monster
    (- (count-pound sea-monster-tile) (* sea-monster-count (length sea-monster)))))
