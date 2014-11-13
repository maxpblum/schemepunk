(define empty-row
    (list
        " "
        " "
        " "
        " "
        " "
        " "
        " "
        " "
        " "
        " "))

(define full-row
  (list
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"
    "*"))

(define empty-grid '((" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ") (" " " " " " " " " " " " " " " " " " " ")))

(define (row-string row)
    (define (iter row sofar)
        (if (null? row)
            sofar
            (iter (cdr row) (string-append sofar (car row)))))
    (string-append "|" (iter row "") "|"))

(define border-row-string (string-append " " (make-string 10 #\-) " \n"))
    
(define (grid-string grid)
  (define (iter grid sofar)
    (if (null? grid)
      sofar
      (iter (cdr grid) (string-append sofar (row-string (car grid)) "\n"))))
  (string-append border-row-string
                 (iter grid "")
                 border-row-string))

(define (print-row row)
    (display (row-string row))
    (newline))

(define (print-grid grid)
  (display (grid-string grid)))

(define (draw-dot x y grid)
  (define (draw-dot-row x row)
    (cond ((< x 0) #f)
          ((null? row) #f)
          ((> x 0)
             (let ((result (draw-dot-row (- x 1) (cdr row))))
               (if result
                 (cons (car row) result)
                 #f)))
          ((string=? (car row) " ")
            (cons "*" (cdr row)))
          (else #f)))
  (cond ((< y 0) #f)
        ((null? grid) #f)
        ((> y 0) 
           (let ((result (draw-dot x (- y 1) (cdr grid))))
             (if result
               (cons (car grid) result)
               #f)))
        (else
           (let ((result (draw-dot-row x (car grid))))
             (if result
               (cons result (cdr grid))
               #f)))))

(define origin-dot-grid
    (draw-dot 0 0 empty-grid))
    
(define (is-empty? x y grid)
  (define (row-is-empty? x row)
    (cond ((null? row) #f)
          ((and (= x 0) (eq? "0" (car row))) #t)
          (else (row-is-empty? (- x 1) (cdr row)))))
  (cond ((null? grid) #f)
        ((= y 0) (row-is-empty? x (car grid)))
        (else (is-empty? x (- y 1) (cdr grid)))))

(define O (list (cons 0 0) (cons 0 -1) (cons 1 0) (cons 1 -1)))
(define I (list (cons -1 0) (cons 0 0) (cons 1 0) (cons 2 0)))
(define T (list (cons -1 0) (cons 0 0) (cons 1 0) (cons -1 -1)))
(define J (list (cons -1 -1) (cons -1 0) (cons 0 0) (cons 1 0)))
(define L (list (cons -1 0) (cons 0 0) (cons 1 0) (cons 1 -1)))
(define S (list (cons -1 0) (cons 0 0) (cons 0 -1) (cons 1 -1)))
(define Z (list (cons 1 0) (cons 0 0) (cons 0 -1) (cons -1 -1)))

(define (shift-point point dx dy)
    (cons (+ (car point) dx) (+ (cdr point) dy)))

(define (multi-shift points dx dy)
    (map (lambda (point) (shift-point point dx dy)) points))

(define (draw-points points grid)
  (cond ((null? points) grid)
        ((not grid) #f)
        (else (let ((result (draw-points (cdr points) (draw-dot (caar points) (cdar points) grid)))) 
          (if result
            result
            #f)))))

(define (rotate-right-point point)
    (cons (- 0 (cdr point)) (car point)))

(define (rotate-left-point point)
    (cons (cdr point) (- 0 (car point))))

(define (rotate-right shape)
    (map rotate-right-point shape))

(define (rotate-left shape)
    (map rotate-left-point shape))

(define example-state
  (list (rotate-right (rotate-right J)) 7 11 empty-grid))
 
(define (shape state) (car state))
(define (shape-dx state) (cadr state)) 
(define (shape-dy state) (caddr state))
(define (grid state) (cadddr state))
  
(define (print-state state)
  (begin
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (newline)
    (print-grid (draw-points (multi-shift (shape state) (shape-dx state) (shape-dy state)) (grid state)))))

(define (move-down state)
  (if (draw-points (multi-shift (shape state) (shape-dx state) (+ (shape-dy state) 1)) (grid state))
    (list (shape state) (shape-dx state) (+ (shape-dy state) 1) (grid state))
    (let ((frozen-grid (draw-points (multi-shift (shape state) (shape-dx state) (shape-dy state)) (grid state))))
      (list '() 0 0 (clear-rows frozen-grid)))))

(define (move-side dir state)
  (if (draw-points (multi-shift (shape state) (+ (shape-dx state) dir) (shape-dy state)) (grid state))
    (list (shape state) (+ (shape-dx state) dir) (shape-dy state) (grid state))
    #f))

(define (move-left state)
  (move-side -1 state))
 
(define (move-right state)
  (move-side 1 state))
 
(define (rotation state)
  (if (draw-points (multi-shift (rotate-right (shape state)) (shape-dx state) (shape-dy state)) (grid state))
    (list (rotate-right (shape state)) (shape-dx state) (shape-dy state) (grid state))
    #f))

(define (move move-fn state)
  (let ((result (move-fn state)))
    (let ((new-state (if result result state)))
      (if (null? (shape state))
        (list (random-shape) 5 1 (grid new-state))
        new-state))))

(define (fn-from-input)
  (let ((input (read)))
    (cond ((eq? input 'j) move-left)
          ((eq? input 'k) move-down)
          ((eq? input 'l) move-right)
          ((eq? input 'i) rotation)
          (else (fn-from-input)))))

(define (random-shape)
  (let ((num (random-integer 7)))
    (cond ((= num 0) O)
          ((= num 1) I)
          ((= num 2) S)
          ((= num 3) Z)
          ((= num 4) J)
          ((= num 5) L)
          ((= num 6) T))))

(define (begin-state) (list (random-shape) 5 1 empty-grid))

(define (clear-rows grid)
  (define (check-row row)
    (cond ((null? row) #t)
          ((not (string=? (car row) "*")) #f)
          (else (check-row (cdr row)))))
  (define (iter grid reversed cleared)
    (cond ((null? grid) (cons (reverse reversed) cleared))
          ((check-row (car grid)) (iter (cdr grid) reversed (+ cleared 1)))
          (else (iter (cdr grid) (cons (car grid) reversed) cleared))))
  (define (add-empty to-add grid)
    (if (> to-add 0)
      (add-empty (- to-add 1) (cons empty-row grid))
      grid))
  (let ((result (iter grid '() 0)))
    (let ((new-grid (car result))
          (to-add (cdr result)))
      (add-empty to-add new-grid))))

(define (play-game)
  (define (game-loop state)
    (begin
      (print-state state)
      (game-loop (move (fn-from-input) state))))
  (game-loop (begin-state)))
  
(define clear-testing-state
  (list '() 0 0 (list
                  empty-row
                  empty-row
                  empty-row
                  empty-row
                  empty-row
                  empty-row
                  empty-row
                  empty-row
                  empty-row
                  empty-row
                  full-row
                  full-row
                  full-row
                  empty-row
                  empty-row
                  full-row
                  full-row
                  full-row
                  full-row
                  full-row
                  full-row
                  full-row
                  full-row
                  full-row
                  full-row
                  full-row)))
