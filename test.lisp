(defconstant MAP_HEIGHT    15)
(defconstant MAP_WIDTH     15)
(defconstant MAP_DIMENSION 2)


(defun make-map (map-height map-width)
  (setq game_map (make-array (list MAP_HEIGHT MAP_WIDTH)))
  (dotimes (i MAP_HEIGHT)
    (dotimes (j MAP_WIDTH)
      (cond ((= i 0) (setf (aref game_map i j) "-"))
        ((= i (- MAP_HEIGHT 1) ) (setf (aref game_map i j) "-"))
        ((= j 0) (setf (aref game_map i j) "|"))
        ((= j (- MAP_WIDTH 1) ) (setf (aref game_map i j) "|"))
        ((setf (aref game_map i j) " "))
      )
    )
  )
  (return-from make-map game_map)
)

(defun print-game-map (game-map snake_position food_position)
  (screen:clear-window (screen:make-window))
  (dotimes (i MAP_HEIGHT)
    (dotimes (j MAP_WIDTH)
      (cond
        ((and (= i (car food_position)) (= j (cadr food_position ))) (princ "*"))
        ((is-snake-position snake_position i j) (princ "@"))
        (t (princ (aref game-map i j)))
      )
      (princ #\Space)
    )
    (princ #\Newline)
  )
)

(defun is-snake-position (snake_position x y)
  (loop for position in snake_position do
    (if (and (= (car position) x) (= (cadr position) y))
      (return-from is-snake-position t)
    )
  )
  (return-from is-snake-position nil)
)

(defun get-snake-next-moviment ()
  (setq moviment (read))
  (return-from get-snake-next-moviment moviment)
)

(defun move-snake (snake_position moviment)
 (cond ((string-equal moviment "A") (let ((snake-head (car snake_position))) (setf (cadr snake-head) (- (cadr snake-head) 1))) )
       ((string-equal moviment "W") (let ((snake-head (car snake_position))) (setf (car snake-head)  (- (car snake-head ) 1))) )
       ((string-equal moviment "S") (let ((snake-head (car snake_position))) (setf (car snake-head)  (+ (car snake-head ) 1))) )
       ((string-equal moviment "D") (let ((snake-head (car snake_position))) (setf (cadr snake-head) (+ (cadr snake-head) 1))) )
 ) 
)

(defun snake-has-eaten-food (snake_position food_position)
  (if (and (= (car (car snake_position)) (car food_position)) ( = (cadr (car snake_position)) (cadr food_position)))
    (return-from snake-has-eaten-food t)
  )
  (return-from snake-has-eaten-food nil)
)

(defun genarate-randow-position ()
  (setq randow-x (random MAP_HEIGHT))
  (setq randow-y (random MAP_WIDTH ))
  (return-from genarate-randow-position (list randow-x randow-y))
)

(defun grow-snake (snake_position)
  (setq new-snake-head (list (car (car snake_position)) (cadr (car snake_position))))
  (setf (car snake_position) new-snake-head)
)

(defun main()
  (setq snake_position (list (list 5 5)))
  (setq food_position (genarate-randow-position))
  (setq game_map (make-map MAP_HEIGHT MAP_WIDTH))
  (loop do
    (print-game-map game_map snake_position food_position)

    (setq moviment (get-snake-next-moviment))
    (move-snake snake_position moviment)
    (if (snake-has-eaten-food snake_position food_position)
      (grow-snake snake_position)
    )
  )
)

(main)
