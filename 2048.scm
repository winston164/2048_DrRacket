(require graphics/graphics)
(open-graphics)
(define view 0)
(define base 2)
(define in 0)
(define out 0)
(define serverInputValue "")
(define oponentReady? #f)
(define oponentMoves 0)
(define (RGB r g b)
  (make-rgb (/ r 255) (/ g 255) (/ b 255))
  )
;matrix-ref: Devuelve un valor en la posición (x,y) de una matris 2D
;parametros: matrix(vectror de vectores) x y(entros)
(define (matrix-ref matrix x y)
  (if (and (>= x 0) (>= y 0)(< x (vector-length matrix)) (< y (vector-length (vector-ref matrix x))) )
      (vector-ref (vector-ref matrix x) y)
      null
  )
  )
;matrix-set: Modifica un valor en la posición (x,y) de una matris 2D
;parametros: matrix(vectror de vectores) x y(enteros) value(valor a poner en la matrís)
(define (matrix-set! matrix x y value)
  (vector-set! (vector-ref matrix x) y value)
  )
;draw-infoBox: dibuja un cuadrado con información del juego
;parametros: posX posY(coordenadas) tag(string de a que informacón refiere) info(la información a desplegar)
(define (draw-infoBox posX posY tag info)
  ((draw-solid-rectangle view) (make-posn posX posY) 140 55 (RGB 187 173 160))
  ((draw-string view) (make-posn (- (+ posX 70) (/ (car ((get-string-size view) tag)) 2)) (+ posY 25)) tag (RGB 214 205 196))
  ((draw-string view) (make-posn (- (+ posX 70) (/ (car ((get-string-size view) info)) 2)) (+ posY 50)) info (RGB 249 246 242))
  )
(define (drawBoard matrix posX posY)
  (define (squareDraw x y color multiplier)
    ((draw-solid-rectangle view) (make-posn (+ posX 20 (* x 80)) (+ posY 20 (* y 80))) 60 60 color);dibujar cuadro de tablero
    (define stringSize ((get-string-size view) (number->string (* multiplier base))))
    (if (> multiplier 2)  ;dibujar numero de cuadro de tablero
        ((draw-string view)
         (make-posn (- (+ posX 50 (* x 80)) (/ (car stringSize) 2)) (+ posY 55 (* y 80)))
         (number->string (* multiplier base))
         (RGB 249 246 242))
        (if (not (= multiplier 0))
            ((draw-string view)
             (make-posn (- (+ posX 50 (* x 80)) (/ (car stringSize) 2)) (+ posY 55 (* y 80)))
             (number->string (* multiplier base))
             (RGB 119 110 101))
            )
        )
    )
  ((draw-solid-rectangle view) (make-posn posX posY) 340 340 (RGB 187 173 160));dibujar tablero
  (let getX
    ([x 0])
    (define value 0)
    (define multiplier 0)
    (if (< x (vector-length matrix))
        (begin
          (let getY
            ([y 0])
            (if (< y (vector-length (vector-ref matrix x)))
                (begin
                  (set! value (matrix-ref matrix x y))
                  (set! multiplier (/ value base))
                  (cond
                    [(= multiplier 1) (squareDraw x y (RGB 238 228 218) multiplier)]
                    [(= multiplier 2) (squareDraw x y (RGB 237 224 200) multiplier)]
                    [(= multiplier 4) (squareDraw x y (RGB 242 177 121) multiplier)]
                    [(= multiplier 8) (squareDraw x y (RGB 245 149 99) multiplier)]
                    [(= multiplier 16) (squareDraw x y (RGB 246 124 95) multiplier)]
                    [(= multiplier 32) (squareDraw x y (RGB 246 94 59) multiplier)]
                    [(= multiplier 64) (squareDraw x y (RGB 237 207 114) multiplier)]
                    [(= multiplier 128) (squareDraw x y (RGB 237 204 97) multiplier)]
                    [(= multiplier 256) (squareDraw x y (RGB 237 200 80) multiplier)]
                    [(= multiplier 512) (squareDraw x y (RGB 237 198 70) multiplier)]
                    [(= multiplier 1024) (squareDraw x y (RGB 237 194 46) multiplier)]
                    [else (squareDraw x y (RGB 214 205 196) multiplier)]
                    )
                  (getY (+ y 1))
                  )
                )
            )
          (getX (+ x 1))
          )
        )
    )
  
  )
;serverConnect: función para conectarse al servidor 
(define (serverConnect)
  (display "IP del servidor: ")
  (define serverIp (read-line))
  (display "Puerto: ")
  (define serverPort (read-line))
  (display "conectando..")
  (tcp-connect serverIp (string->number serverPort))
)

;serverInput: Subrutina para recivir inputs del servidor
(define (serverInput)
  (let getInput
    ([input (read in)])
    (if (not (equal? input serverInputValue))
        (begin
          (set! serverInputValue input)
          (write "recieved" out) (newline out)
          (flush-output out)
          (if (number? input)
              (set! oponentMoves input)
              )
          )
        )
    (getInput (read in))
    )
  )

;serverOutput: Subrutina para mandar mensajes al servidor
(define (serverOutput value)
          (write value out) (newline out)
          (flush-output out)
  )

;menu: despliega un menú en el viewport que al seleccionar una opción devuelve dicha opción
(define (menu)
  ((draw-solid-rectangle view) (make-posn 500 20) 300 60 (RGB 237 194 46))
  ((draw-string view) (make-posn (- 650 (/ (car ((get-string-size view) "2048")) 2)) 55) "2048" (RGB 249 246 242))
  ((draw-string view) (make-posn (- 650 (/ (car ((get-string-size view) "Escoger oponente")) 2)) 115) "Escoger oponente" (RGB 119 110 101))
  ((draw-solid-rectangle view) (make-posn 550 135) 200 60 (RGB 237 207 114))
  ((draw-string view) (make-posn (- 650 (/ (car ((get-string-size view) "Jugador")) 2)) 170) "Jugador" (RGB 249 246 242))
  ((draw-solid-rectangle view) (make-posn 550 215) 200 60 (RGB 237 224 200))
  ((draw-string view) (make-posn (- 650 (/ (car ((get-string-size view) "Máquina")) 2)) 250) "Máquina" (RGB 119 110 101))
  (let waitClick
    ([mc (mouse-click-posn (get-mouse-click view))])
    (define mouseX (posn-x mc))
    (define mouseY (posn-y mc))
    (define clickedButton? #f)
    (cond
      [(and (<= mouseX 750) (>= mouseX 550) (<= mouseY 195) (>= mouseY 135))
       "player"]
      [(and (<= mouseX 750) (>= mouseX 550) (<= mouseY 275) (>= mouseY 215))
       "machine"]
      [else
       (waitClick (mouse-click-posn (get-mouse-click view)))])
    )
   )
;oponentGame: subrutina que dibuja el tablero y la información del oponente
(define (oponentGame)
  (define gameMatrix (vector))
  (define (greatestNumber);devuelve el número mayor de la matrís
    (define answer 0)
    (define value 0)
    (let getX
      ([x 0])
      (if (< x (vector-length gameMatrix))
          (begin
            (let getY
              ([y 0])
              (if (< y (vector-length (vector-ref gameMatrix x)))
                  (begin
                    (set! value (matrix-ref gameMatrix x y))
                    (if (> value answer)
                        (set! answer value)
                        )
                    (getY (+ y 1))
                    )
                  )
              )
            (getX (+ x 1))
            )
          )
      )
    answer
    )
  (let getOponentReady ()
      (if (and (string? serverInputValue) (equal? "ready" serverInputValue))
          (begin
            (set! oponentReady? #t)
            (display "oponent ready")
            )
          (getOponentReady)
          )
      )
  (sleep 2)
  ((draw-solid-rectangle view) (make-posn 1000 20) 140 60 (RGB 242 177 121))
  ((draw-string view) (make-posn (- 1070 (/ (car ((get-string-size view) "Oponente")) 2)) 55) "Oponente" (RGB 249 246 242))
  (define movesMade -1)
  (let play
    ()
    (let getGameMatrix ()
      (if (and (vector? serverInputValue)(not (equal? gameMatrix serverInputValue)))
          (set! gameMatrix serverInputValue)
          (getGameMatrix)
          )
      )
    (set! movesMade oponentMoves)
    (define gNumber (greatestNumber))
    (draw-infoBox 720 100 "Número Mayor" (number->string gNumber))
    (draw-infoBox 720 175 "Movimientos" (number->string (+ 1 movesMade)))
    (drawBoard gameMatrix 900 100)
    (play)
    )
  )

;playGame: Jugar en el tablero con WASD
(define (playGame)
  (define gameMatrix (vector (vector 0 0 0 0) (vector 0 0 0 0) (vector 0 0 0 0) (vector 0 0 0 0)));Matriz principal de 4x4
  (define (moveAvailable?);devuelve verdadero si es posible hacer un movimiento
    (define answer #f)
    (define value 0)
    (let getX
      ([x 0])
      (if (and (< x (vector-length gameMatrix)) (not answer))
          (begin
            (let getY
              ([y 0])
              (if (and (< y (vector-length (vector-ref gameMatrix x))) (not answer))
                  (begin
                    (set! value (matrix-ref gameMatrix x y))
                    (set! answer (or
                                  (equal? (matrix-ref gameMatrix (- x 1) y) value) (equal? (matrix-ref gameMatrix (- x 1) y) 0)
                                  (equal? (matrix-ref gameMatrix (+ x 1) y) value) (equal? (matrix-ref gameMatrix (+ x 1) y) 0)
                                  (equal? (matrix-ref gameMatrix x (- y 1)) value) (equal? (matrix-ref gameMatrix x (- y 1)) 0)
                                  (equal? (matrix-ref gameMatrix x (+ y 1)) value) (equal? (matrix-ref gameMatrix x (+ y 1)) 0)))
                    (getY (+ y 1))
                    )
                  )
              )
            (getX (+ x 1))
            )
          )
      )
    answer
    )
  (define (greatestNumber);devuelve el número mayor de la matrís
    (define answer 0)
    (define value 0)
    (let getX
      ([x 0])
      (if (< x (vector-length gameMatrix))
          (begin
            (let getY
              ([y 0])
              (if (< y (vector-length (vector-ref gameMatrix x)))
                  (begin
                    (set! value (matrix-ref gameMatrix x y))
                    (if (> value answer)
                        (set! answer value)
                        )
                    (getY (+ y 1))
                    )
                  )
              )
            (getX (+ x 1))
            )
          )
      )
    answer
    )
  (define (blankSpaces) ;devuelve la cantidad de casillas vasias en la matrís
    (define answer 0)
    (define value 0)
    (let getX
      ([x 0])
      (if (< x (vector-length gameMatrix))
          (begin
            (let getY
              ([y 0])
              (if (< y (vector-length (vector-ref gameMatrix x)))
                  (begin
                    (set! value (matrix-ref gameMatrix x y))
                    (if (equal? value 0)
                        (set! answer (+ answer 1))
                        )
                    (getY (+ y 1))
                    )
                  )
              )
            (getX (+ x 1))
            )
          )
      )
    answer
    )
  (define (fillRandom);llena un espacio aleatorio de la matriz
    (let cicle
      ([x (random 4)]
      [y (random 4)])
      (if (equal? 0 (matrix-ref gameMatrix x y))
          (if (equal? 0 (random 10))
              (matrix-set! gameMatrix x y (* base 2))
              (matrix-set! gameMatrix x y base)
              )
          (cicle (random 4) (random 4))
          )
      )
    )
  (define exit? #f)
  (define (exitButton)
    ((draw-solid-rectangle view) (make-posn 560 100) 140 60 (RGB 246 94 59))
    ((draw-string view) (make-posn (- 630 (/ (car ((get-string-size view) "Rendirse")) 2)) 135) "Rendirse" (RGB 249 246 242))
    (display "button thread active")
    (let getClick
      ([mc  (get-mouse-click view)])
      (define mouseX (posn-x (mouse-click-posn mc)))
      (define mouseY (posn-y (mouse-click-posn mc)))
      (if (and (<= mouseX 700) (>= mouseX 560) (<= mouseY 160) (>= mouseY 100))
          (set! exit? #t)
          )
      (getClick (get-mouse-click view))
      ) 
    )
  (define exitButtonThread (thread exitButton))
  ((draw-solid-rectangle view) (make-posn 300 20) 140 60 (RGB 237 194 46))
  ((draw-string view) (make-posn (- 370 (/ (car ((get-string-size view) "Tu")) 2)) 55) "Tu" (RGB 249 246 242))
  (fillRandom)
  (fillRandom)
  (drawBoard gameMatrix 200 100)
  (define movesMade 0)
  (serverOutput gameMatrix)
  (define turns 0)
  (define (mainGame)
  (let play
   ([turnCount 0]
   [previousMove ""])
    (define kp #\ )
    (define currentMove previousMove)
    (define gNumber (greatestNumber))
    (define moveDone? #f)
    (draw-infoBox 30 100 "Tiro anterior" previousMove)
    (draw-infoBox 30 175 "Casillas vacias" (number->string (blankSpaces)))
    (draw-infoBox 30 250 "Movimientos" (number->string turnCount))
    (draw-infoBox 30 325 "Número mayor" (number->string gNumber))
    (if (and (moveAvailable?) (not (equal? (* base 1024) gNumber)))
        (begin
          (set! kp (key-value (get-key-press view)))
          (cond
            [(or (equal? kp #\w) (equal? kp #\W))
             (let getX
               ([x 0])
               (if (< x (vector-length gameMatrix))
                   (begin
                     (let getY
                       ([y 0])
                       (if (< y (vector-length (vector-ref gameMatrix x)))
                           (begin
                             (if (and (equal? (matrix-ref gameMatrix x (+ y 1)) (matrix-ref gameMatrix x y)) (not (equal? (matrix-ref gameMatrix x y) 0)))
                                 (begin
                                   (matrix-set! gameMatrix x y (* 2 (matrix-ref gameMatrix x y)))
                                   (matrix-set! gameMatrix x (+ y 1) 0)
                                   (set! moveDone? #t)
                                   (set! currentMove "arriba")
                                   )
                                 )
                             (if (and (equal? (matrix-ref gameMatrix x (- y 1)) 0) (not (equal? (matrix-ref gameMatrix x y) 0)))
                                 (begin
                                   (set! moveDone? #t)
                                   (set! currentMove "arriba")
                                   (if (equal? (matrix-ref gameMatrix x (- y 2)) 0)
                                       (if (equal? (matrix-ref gameMatrix x (- y 3)) 0)
                                           (begin
                                             (matrix-set! gameMatrix x (- y 3) (matrix-ref gameMatrix x y))
                                             (matrix-set! gameMatrix x y 0)
                                             )
                                           (begin
                                             (matrix-set! gameMatrix x (- y 2) (matrix-ref gameMatrix x y))
                                             (matrix-set! gameMatrix x y 0)
                                             )
                                           )
                                       (begin
                                         (matrix-set! gameMatrix x (- y 1) (matrix-ref gameMatrix x y))
                                         (matrix-set! gameMatrix x y 0)
                                         )
                                       )
                                   )
                                 )
                             (getY (+ y 1))
                             )
                           )
                       )
                     (getX (+ x 1))
                     )
                   )
               )
             ]
            [(or (equal? kp #\s) (equal? kp #\S))
             (let getX
               ([x 0])
               (if (< x (vector-length gameMatrix))
                   (begin
                     (let getY
                       ([y (- (vector-length (vector-ref gameMatrix x)) 1)])
                       (if (>= y 0)
                           (begin
                             (if (and (equal? (matrix-ref gameMatrix x (- y 1)) (matrix-ref gameMatrix x y)) (not (equal? (matrix-ref gameMatrix x y) 0)))
                                 (begin
                                   (matrix-set! gameMatrix x y (* 2 (matrix-ref gameMatrix x y)))
                                   (matrix-set! gameMatrix x (- y 1) 0)
                                   (set! moveDone? #t)
                                   (set! currentMove "abajo")
                                   )
                                 )
                             (if (and (equal? (matrix-ref gameMatrix x (+ y 1)) 0) (not (equal? (matrix-ref gameMatrix x y) 0)))
                                 (begin
                                   (set! moveDone? #t)
                                   (set! currentMove "abajo")
                                   (if (equal? (matrix-ref gameMatrix x (+ y 2)) 0)
                                       (if (equal? (matrix-ref gameMatrix x (+ y 3)) 0)
                                           (begin
                                             (matrix-set! gameMatrix x (+ y 3) (matrix-ref gameMatrix x y))
                                             (matrix-set! gameMatrix x y 0)
                                             )
                                           (begin
                                             (matrix-set! gameMatrix x (+ y 2) (matrix-ref gameMatrix x y))
                                             (matrix-set! gameMatrix x y 0)
                                             )
                                           )
                                       (begin
                                         (matrix-set! gameMatrix x (+ y 1) (matrix-ref gameMatrix x y))
                                         (matrix-set! gameMatrix x y 0)
                                         )
                                       )
                                   )
                                 )
                             (getY (- y 1))
                             )
                           )
                       )
                     (getX (+ x 1))
                     )
                   )
               )
             ]
            [(or (equal? kp #\d) (equal? kp #\D))
             (let getY
               ([y 0])
               (if (< y 4)
                   (begin
                     (let getX
                       ([x 3])
                       (if (>= x 0)
                           (begin
                             (if (and (equal? (matrix-ref gameMatrix (- x 1) y) (matrix-ref gameMatrix x y)) (not (equal? (matrix-ref gameMatrix x y) 0)))
                                 (begin
                                   (matrix-set! gameMatrix x y (* 2 (matrix-ref gameMatrix x y)))
                                   (matrix-set! gameMatrix (- x 1) y 0)
                                   (set! moveDone? #t)
                                   (set! currentMove "derecha")
                                   )
                                 )
                             (if (and (equal? (matrix-ref gameMatrix (+ x 1) y) 0) (not (equal? (matrix-ref gameMatrix x y) 0)))
                                 (begin
                                   (set! moveDone? #t)
                                   (set! currentMove "derecha")
                                   (if (equal? (matrix-ref gameMatrix (+ x 2) y) 0)
                                       (if (equal? (matrix-ref gameMatrix (+ x 3) y) 0)
                                           (begin
                                             (matrix-set! gameMatrix (+ x 3) y (matrix-ref gameMatrix x y))
                                             (matrix-set! gameMatrix x y 0)
                                             )
                                           (begin
                                             (matrix-set! gameMatrix (+ x 2) y (matrix-ref gameMatrix x y))
                                             (matrix-set! gameMatrix x y 0)
                                             )
                                           )
                                       (begin
                                         (matrix-set! gameMatrix (+ x 1) y (matrix-ref gameMatrix x y))
                                         (matrix-set! gameMatrix x y 0)
                                         )
                                       )
                                   )
                                 )
                             (getX (- x 1))
                             )
                           )
                       )
                     (getY (+ y 1))
                     )
                   )
               )
             ]
            [(or (equal? kp #\a) (equal? kp #\A))
             (let getY
               ([y 0])
               (if (< y 4)
                   (begin
                     (let getX
                       ([x 0])
                       (if (< x 4)
                           (begin
                             (if (and (equal? (matrix-ref gameMatrix (+ x 1) y) (matrix-ref gameMatrix x y)) (not (equal? (matrix-ref gameMatrix x y) 0)))
                                 (begin
                                   (matrix-set! gameMatrix x y (* 2 (matrix-ref gameMatrix x y)))
                                   (matrix-set! gameMatrix (+ x 1) y 0)
                                   (set! moveDone? #t)
                                   (set! currentMove "izquierda")
                                   )
                                 )
                             (if (and (equal? (matrix-ref gameMatrix (- x 1) y) 0) (not (equal? (matrix-ref gameMatrix x y) 0)))
                                 (begin
                                   (set! moveDone? #t)
                                   (set! currentMove "izquierda")
                                   (if (equal? (matrix-ref gameMatrix (- x 2) y) 0)
                                       (if (equal? (matrix-ref gameMatrix (- x 3) y) 0)
                                           (begin
                                             (matrix-set! gameMatrix (- x 3) y (matrix-ref gameMatrix x y))
                                             (matrix-set! gameMatrix x y 0)
                                             )
                                           (begin
                                             (matrix-set! gameMatrix (- x 2) y (matrix-ref gameMatrix x y))
                                             (matrix-set! gameMatrix x y 0)
                                             )
                                           )
                                       (begin
                                         (matrix-set! gameMatrix (- x 1) y (matrix-ref gameMatrix x y))
                                         (matrix-set! gameMatrix x y 0)
                                         )
                                       )
                                   )
                                 )
                             (getX (+ x 1))
                             )
                           )
                       )
                     (getY (+ y 1))
                     )
                   )
               )
             ]
            )
          (if moveDone?
              (begin
                (fillRandom)
                (drawBoard gameMatrix 200 100)
                (serverOutput gameMatrix)
                (sleep 0.25)
                (serverOutput (+ 1 turnCount))
                (set! movesMade (+ 1 turnCount))
                (play (+ 1 turnCount) currentMove)
              )
              (play turnCount currentMove)
              )
          )
        (set! exit? #t)
        )
    )
    1
  )
  (define mainGameThread (thread mainGame))
  (let cicle()
    (if (not exit?)
        (cicle)
        )
    )
  (kill-thread mainGameThread)
  (kill-thread exitButtonThread)
  (values (greatestNumber) movesMade)
  )


;Main: subrutiona principal donde correrá el programa
  (set!-values (in out) (serverConnect))
  (display "Conexion exitosa!")(sleep 1)
  (set! view (open-viewport "2048" 1300 500))
  ((draw-viewport view) (RGB 249 246 242))
  (define inputThread (thread serverInput))
  (define mode (menu))
  (let getBase ()
    (if (number? serverInputValue)
        (begin
          (set! base serverInputValue)
          (serverOutput "Base recieved")
          )
        (getBase)
        )
    )
  (define oponentThread (thread oponentGame))
  (display "thread made")
  (let waitOponentReady ([dots 0])
    (if (not oponentReady?)
        (begin
          (cond
            [(equal? dots 0)
             ((draw-string view) (make-posn (- 650 (/ (car ((get-string-size view) "Esperando..")) 2)) 415) "Esperando" (RGB 119 110 101))
             ]
            [(equal? dots 1)
             ((draw-string view) (make-posn (- 650 (/ (car ((get-string-size view) "Esperando..")) 2)) 415) "Esperando." (RGB 119 110 101))
             ]
            [(equal? dots 2)
             ((draw-string view) (make-posn (- 650 (/ (car ((get-string-size view) "Esperando..")) 2)) 415) "Esperando.." (RGB 119 110 101))
             ]
            [(equal? dots 3)
             ((draw-string view) (make-posn (- 650 (/ (car ((get-string-size view) "Esperando..")) 2)) 415) "Esperando..." (RGB 119 110 101))
             (set! dots -1)]
            )
          (sleep 1)
          ((draw-string view) (make-posn (- 650 (/ (car ((get-string-size view) "Esperando..")) 2)) 415) "Esperando..." (RGB 249 246 242))
          (waitOponentReady (+ dots 1))
        )
        )
    )
  ((draw-viewport view) (RGB 249 246 242))
  (define-values (playerGreatestNumber playerMoves) (playGame))
  (serverOutput (cons playerGreatestNumber playerMoves))
  (let waitResult([dots 0])
    (if (or (equal? serverInputValue "Winner") (equal? serverInputValue "Loser") (equal? serverInputValue "Tie"))
        (begin
          ((draw-string view) (make-posn 200  460) "Esperando que termine el oponente..." (RGB 249 246 242))
          (cond
            [(equal? serverInputValue "Winner")
             ((draw-string view) (make-posn 200  460) "Ganaste!" (RGB 119 110 101))
             ]
            [(equal? serverInputValue "Loser")
             ((draw-string view) (make-posn 200  460) "Perdiste =( mejor suerte a la proxima" (RGB 119 110 101))
             ]
            [(equal? serverInputValue "Tie")
             ((draw-string view) (make-posn 200  460) "Empate!" (RGB 119 110 101))
             ]
            )
          )
        (begin
          ((draw-string view) (make-posn 200  460) "Esperando que termine el oponente..." (RGB 249 246 242))
          (cond
            [(equal? dots 0)
             ((draw-string view) (make-posn 200  460) "Esperando que termine el oponente" (RGB 119 110 101))
             ]
            [(equal? dots 1)
             ((draw-string view) (make-posn 200  460) "Esperando que termine el oponente." (RGB 119 110 101))
             ]
            [(equal? dots 2)
             ((draw-string view) (make-posn 200  460) "Esperando que termine el oponente.." (RGB 119 110 101))
             ]
            [(equal? dots 3)
             ((draw-string view) (make-posn 200  460) "Esperando que termine el oponente..." (RGB 119 110 101))
             (set! dots -1)]
            )
            (sleep 1)
          (waitResult (+ 1 dots))
          )
        )
    )
  ((draw-string view) (make-posn 200  480) "Presione cualquier botón para salir" (RGB 119 110 101))
  (viewport-flush-input view)
  (get-key-press view)
  (kill-thread inputThread)
  (kill-thread oponentThread)
  (close-graphics)