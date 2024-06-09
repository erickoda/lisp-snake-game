(setf *random-state* (make-random-state t))
(defconstant MAP_HEIGHT    10)
(defconstant MAP_WIDTH     10)
(defconstant MAP_DIMENSION  2)

; this function will setup the base map of the game
; the map will be a matrix with the height and width of the game
; the map will have the walls and the empty spaces
; the walls will be represented by the characters "-" and "|"
; the empty spaces will be represented by the character " "
; the snake and the food will only be displayed in the empty spaces
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


; this function will print the game map
; the game map will be overlaid with the snake and the food
(defun print-game-map (game-map snake_position snake_size food_position)
  (screen:clear-window (screen:make-window))
  (dotimes (i MAP_HEIGHT)
    (dotimes (j MAP_WIDTH)
      (cond
        ((and (= i (car food_position)) (= j (cadr food_position ))) (princ "*"))
        ((is-snake-position snake_position snake_size i j) (princ "@"))
        (t (princ (aref game-map i j)))
      )
      (princ #\Space)
    )
    (princ #\Newline)
  )
)

(defun is-snake-position (snake_position snake_size x y)
  (dotimes (i snake_size)
    (if (and (= (aref snake_position i 0) x) (= (aref snake_position i 1) y))
      (return-from is-snake-position t)
    )
  )
  (return-from is-snake-position nil)
)

(defun get-snake-next-moviment ()
  (setq moviment (read))
  (return-from get-snake-next-moviment moviment)
)

; this function will move the snake
; this function updates all history positions of the snake
(defun move-snake (snake_position moviment)
  (loop for i downfrom (- (* MAP_HEIGHT MAP_WIDTH) 1) to 1 by 1 do
    (setf (aref snake_position i 0) (aref snake_position (- i 1) 0))
    (setf (aref snake_position i 1) (aref snake_position (- i 1) 1))
  )
  (cond ((string-equal moviment "A") (setf (aref snake_position 0 1) (- (aref snake_position 0 1) 1) ))
        ((string-equal moviment "W") (setf (aref snake_position 0 0) (- (aref snake_position 0 0) 1) ))
        ((string-equal moviment "S") (setf (aref snake_position 0 0) (+ (aref snake_position 0 0) 1) ))
        ((string-equal moviment "D") (setf (aref snake_position 0 1) (+ (aref snake_position 0 1) 1) ))
  )
)

(defun snake-has-eaten-food (snake_position food_position)
  (if (and (= (aref snake_position 0 0) (car food_position)) ( = (aref snake_position 0 1) (cadr food_position)))
    (return-from snake-has-eaten-food t)
  )
  (return-from snake-has-eaten-food nil)
)

; this function generates a random position for the food
; if the food is generated in a position that is already occupied by the snake or the wall
; the function will generate a new position for the food
(defun genarate-randow-food-position (snake_position snake_size)
  (setq randow-x (random MAP_HEIGHT))
  (setq randow-y (random MAP_WIDTH ))
  (cond ((is-snake-position snake_position snake_size randow-x randow-y) (return-from genarate-randow-food-position (genarate-randow-food-position snake_position snake_size)))
        ((= randow-x 0) (return-from genarate-randow-food-position (genarate-randow-food-position snake_position snake_size)))
        ((= randow-y 0) (return-from genarate-randow-food-position (genarate-randow-food-position snake_position snake_size)))
        ((= randow-x (- MAP_HEIGHT 1)) (return-from genarate-randow-food-position (genarate-randow-food-position snake_position snake_size)))
        ((= randow-y (- MAP_WIDTH 1)) (return-from genarate-randow-food-position (genarate-randow-food-position snake_position snake_size)))
  )
  (return-from genarate-randow-food-position (list randow-x randow-y))
)

(defun is-moviment-valid (moviment)
  (cond ((string-equal moviment "A") (return-from is-moviment-valid t))
        ((string-equal moviment "W") (return-from is-moviment-valid t))
        ((string-equal moviment "S") (return-from is-moviment-valid t))
        ((string-equal moviment "D") (return-from is-moviment-valid t))
  )
  (return-from is-moviment-valid nil)
)

; the snake will be dead if it hits the wall or itself
(defun is-snake-dead (snake_position moviment snake_size)
  (cond ((string-equal moviment "A") (or (= (+ (aref snake_position 0 1) -1) 0) (is-snake-position snake_position snake_size (aref snake_position 0 0) (+ (aref snake_position 0 1) -1)) ))
        ((string-equal moviment "W") (or (= (+ (aref snake_position 0 0) -1) 0) (is-snake-position snake_position snake_size (+ (aref snake_position 0 0) -1) (aref snake_position 0 1)) ))
        ((string-equal moviment "S") (or (= (+ (aref snake_position 0 0) 1) (- MAP_HEIGHT 1)) (is-snake-position snake_position snake_size (+ (aref snake_position 0 0) 1) (aref snake_position 0 1)) ) )
        ((string-equal moviment "D") (or (= (+ (aref snake_position 0 1) 1) (- MAP_WIDTH 1)) (is-snake-position snake_position snake_size (aref snake_position 0 0) (+ (aref snake_position 0 1) 1))  ) )
  )
)

(defun update-score (score)
  (setq score (+ score 100))
)

(defun grow-snake (snake_size)
  (setq snake_size (+ snake_size 1))
)

(defun print-game-over-screnn (score)
  (screen:clear-window (screen:make-window))
  (princ "Your Snake Died!!!")
  (princ #\Newline)
  (princ " 
      /$$$$$$   /$$$$$$  /$$      /$$ /$$$$$$$$        /$$$$$$  /$$    /$$ /$$$$$$$$ /$$$$$$$ 
     /$$__  $$ /$$__  $$| $$$    /$$$| $$_____/       /$$__  $$| $$   | $$| $$_____/| $$__  $$
    | $$  \\__/| $$  \\ $$| $$$$  /$$$$| $$            | $$  \\ $$| $$   | $$| $$      | $$  \\ $$
    | $$ /$$$$| $$$$$$$$| $$ $$/$$ $$| $$$$$         | $$  | $$|  $$ / $$/| $$$$$   | $$$$$$$/
    | $$|_  $$| $$__  $$| $$  $$$| $$| $$__/         | $$  | $$ \\  $$ $$/ | $$__/   | $$__  $$
    | $$  \\ $$| $$  | $$| $$\\  $ | $$| $$            | $$  | $$  \\  $$$/  | $$      | $$  \\ $$
    |  $$$$$$/| $$  | $$| $$ \\/  | $$| $$$$$$$$      |  $$$$$$/   \\  $/   | $$$$$$$$| $$  | $$
     \\______/ |__/  |__/|__/     |__/|________/       \\______/     \\_/    |________/|__/  |__/
 ")
  (princ #\Newline)
  (princ "Score: ")
  (princ score)
)

(defun print-intial-game-screen ()
  (screen:clear-window (screen:make-window))
  (princ "
     $$$$$$\\  $$\\   $$\\  $$$$$$\\  $$\\   $$\\ $$$$$$$$\\        $$$$$$\\   $$$$$$\\  $$\\      $$\\ $$$$$$$$\\ 
    $$  __$$\\ $$$\\  $$ |$$  __$$\\ $$ | $$  |$$  _____|      $$  __$$\\ $$  __$$\\ $$$\\    $$$ |$$  _____|
    $$ /  \\__|$$$$\\ $$ |$$ /  $$ |$$ |$$  / $$ |            $$ /  \\__|$$ /  $$ |$$$$\\  $$$$ |$$ |      
    \\$$$$$$\\  $$ $$\\$$ |$$$$$$$$ |$$$$$  /  $$$$$\\          $$ |$$$$\\ $$$$$$$$ |$$\\$$\\$$ $$ |$$$$$\\    
     \\____$$\\ $$ \\$$$$ |$$  __$$ |$$  $$<   $$  __|         $$ |\\_$$ |$$  __$$ |$$ \\$$$  $$ |$$  __|   
    $$\\   $$ |$$ |\\$$$ |$$ |  $$ |$$ |\\$$\\  $$ |            $$ |  $$ |$$ |  $$ |$$ |\\$  /$$ |$$ |      
    \\$$$$$$  |$$ | \\$$ |$$ |  $$ |$$ | \\$$\\ $$$$$$$$\\       \\$$$$$$  |$$ |  $$ |$$ | \\_/ $$ |$$$$$$$$\\ 
    \\______/ \\__|  \\__|\\__|  \\__|\\__|  \\__|\\________|       \\______/ \\__|  \\__|\\__|     \\__|\\________|
  ")
  (princ #\Newline)
  (princ #\Newline)
  (princ "Use the keys W, A, S, D to move the snake")
  (princ #\Newline)
  (princ "Press any key to start the game")
  (read)
)

(defun main()
  (setq snake_position (make-array (list (* MAP_HEIGHT MAP_WIDTH) 2) :initial-element 1 ) )
  (setq snake_size 1)
  (setq food_position (genarate-randow-food-position snake_position snake_size))
  (setq game_map (make-map MAP_HEIGHT MAP_WIDTH))
  (setq score 0)

  (print-intial-game-screen)

  (loop do
    (print-game-map game_map snake_position snake_size food_position)

    (setq moviment (get-snake-next-moviment))

    (if (is-snake-dead snake_position moviment snake_size)
      (progn
        (print-game-over-screnn score)
        (return-from main)
      )
    )

    (if (is-moviment-valid moviment)
      (progn
        (move-snake snake_position moviment)
        (if (snake-has-eaten-food snake_position food_position)
          (progn
            (setq score (update-score score))
            (setq snake_size (grow-snake snake_size))
            (setq food_position (genarate-randow-food-position snake_position snake_size))
          )
        )
      )
    )
  )
)

(main)
