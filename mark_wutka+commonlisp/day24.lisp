
;;; Turn the string into a list, if the next char is s or n, also take the
;;; character after it, recursively parse the line
(defun parse-line (line-str)
  (labels ((parse-line-rec (line)	     
	     (if (= 0 (length line)) '()
		 (if (or (char= (car line) #\s)
			 (char= (car line) #\n))
		     (cons (map 'string #'identity (list (car line) (cadr line)))
			   (parse-line-rec (cddr line)))
		     (cons (map 'string #'identity (list(car line)))
			   (parse-line-rec (cdr line)))))))
    (parse-line-rec (map 'list #'identity line-str))))


;;; Treat the hex grid as a regular grid where x runs from nw to se, and
;;; y runs from ne to sw. The grid offsets for the six directions would be:
;;;
;;;                (-1 0)  (0 -1)
;;;          (-1 +1)   (0 0)    (1 -1)
;;;                (0 1)   (1 0)
;;;
;;; There are other ways to do the coordinates, the directions I chose
;;; are somewhat arbitrary
(defun do-move (coord move)
  (let ((x (car coord))
	(y (cadr coord)))
    (cond
      ((string= move "nw") (list (1- x) y))
      ((string= move "ne") (list x (1- y)))
      ((string= move "e") (list (1+ x) (1- y)))
      ((string= move "se") (list (1+ x) y))
      ((string= move "sw") (list x (1+ y)))
      ((string= move "w") (list (1- x) (1+ y))))))

;;; Flip the tile at a particular coord. The map only contains black coords
;;; so to flip a tile, you remove it from the map if it exists, or add it
;;; if it doesn't
(defun flip-coord (coord tile-map)
  (let ((prev-tile (gethash coord tile-map)))
    (if prev-tile
	(remhash coord tile-map)
	(setf (gethash coord tile-map) coord))))

;;; Follow a list of moves to the destination coordinate and flip the
;;; tile at the resulting location
(defun do-move-list (move-list tile-map)
  (labels ((do-move-list-rec (coord move-list)
	      (if (null move-list) coord
		  (do-move-list-rec (do-move coord (car move-list))
		    (cdr move-list)))))
    (flip-coord (do-move-list-rec (list 0 0) move-list)
		tile-map)))

(defun day24a ()
  (let* ((move-lists (mapcar #'parse-line (read-file "day24.txt")))
	 (tile-map (make-hash-table :test #'equalp)))
    ;;; For each list of moves, execute the moves the flip
    (dolist (move-list move-lists)
      (do-move-list move-list tile-map))

    ;;; Count the number of items in the tile map, which will be
    ;;; the number of black tiles
    (loop for k being the hash-keys of tile-map
	 count 1)))

;;; Returns a list of the coordinates of all the black tiles
;;; (i.e. the keys of tile-map
(defun map-coords (tile-map)
  (loop for k being the hash-keys of tile-map
       collect k))

;;; The offsets for the 6 cells surrounding a cell in the grid
(defparameter *offsets* '((-1 0) (0 -1) (1 -1) (1 0) (-1 1) (0 1)))

;;; Count the number of adjacent black tiles for any coordinate
(defun count-adjacent (coord tile-map)
  (let ((x (car coord))
	(y (cadr coord)))
    (apply #'+ (mapcar
		(lambda (off)
		  (let ((off-x (+ (car off) x))
			(off-y (+ (cadr off) y)))
		    (if (gethash (list off-x off-y) tile-map) 1 0)))
		*offsets*))))

;;; Compute the minimum and maximum x and y coordinates from the list of tiles
;;; subtracting 1 from the mins and adding 1 to the maxes to include the tiles
;;; adjacent to the ones at the ends of the ranges. This uses a multiple-value
;;; return instead of returning a list
(defun ranges (tile-map)
  (labels ((ranges-rec (coords min-x max-x min-y max-y)
	     (if (null coords) (values (1- min-x) (1+ max-x) (1- min-y) (1+ max-y))			  
		 (let ((x (caar coords))
		       (y (cadar coords)))
		   (ranges-rec (cdr coords)
			       (min x min-x) (max x max-x)
			       (min y min-y) (max y max-y))))))
    (let* ((coords (map-coords tile-map))
	   (first-coord (car coords))
	   (x (car first-coord))
	   (y (cadr first-coord)))
      (ranges-rec (cdr coords) x x y y))))

;;; Execute the rules for the round at one coordinate, and make the
;;; updates in the new tile map (i.e. if a coordinate is supposed to
;;; be black, add that coordinate to the new tile map
(defun do-round-for-coord (coord tile-map new-tile-map)
  (let ((is-black (gethash coord tile-map))
	(adjacents (count-adjacent coord tile-map)))
    (cond ((and is-black
		(or (= 1 adjacents)
		    (= 2 adjacents)))
	   (setf (gethash coord new-tile-map) coord))
	  ((and (not is-black)
		(= 2 adjacents))
	   (setf (gethash coord new-tile-map) coord)))))

;;; Create a blank tile map (all tiles white), and then iterate
;;; over the x and y ranges, executing the game rules for each x y coord
;;; and store the result in the new tile map
(defun do-round (tile-map)
  (let ((new-tile-map (make-hash-table :test #'equalp)))
    (multiple-value-bind (min-x max-x min-y max-y) (ranges tile-map)
      (loop for x from min-x to max-x
	 do (loop for y from min-y to max-y
	       do (do-round-for-coord (list x y) tile-map new-tile-map))))
    new-tile-map))

;;; Do n rounds of the game, returning the resulting tile map
(defun do-rounds (tile-map n)
  (if (= n 0) tile-map
      (do-rounds (do-round tile-map) (1- n))))

(defun day24b ()
  (let* ((move-lists (mapcar #'parse-line (read-file "day24.txt")))
	 (tile-map (make-hash-table :test #'equalp)))
    (dolist (move-list move-lists)
      (do-move-list move-list tile-map))
    (loop for k being the hash-keys of (do-rounds tile-map 100)
	 count 1)))
