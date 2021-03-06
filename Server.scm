(require racket/tcp)
(display "Base para los clientes")
(define base (read))
(define matrix1 (vector))
(define matrix2 (vector))
(define-values (score1 moves1) (values 0 0))
(define-values (score2 moves2) (values 0 0))
(define player1Ready? #f)
(define player2Ready? #f)
(define player1done? #f)
(define player2done? #f)
(define gameDone? #f)
(define player1Result "")
(define player2Result "")

(define (server1)
  (define listener (tcp-listen 2002))
  (define-values (in out) (tcp-accept listener))
  (define clientInputValue "")
  (define (clientInput)
    (let getInput
      ([input (read in)])
      (if (not (equal? input clientInputValue))
          (begin
            (set! clientInputValue input)
            (if (number? input)
                (set! moves1 input)
                )
            )
      )
      (getInput (read in))
    )
    )
  (define inputThread (thread clientInput))
  (define (clientOutput value)
    (write value out) (newline out)
    (flush-output out)
    )
  (clientOutput base)
  (let ready? ()
    (if (equal? clientInputValue "Base recieved")
        (set! player1Ready? #t)
        (ready?)
        )
    )
  (let oponentReady? ()
    (if player2Ready?
        (clientOutput "ready")
        (oponentReady?)
        )
    )
  (let defineMatrix ()
    (if (vector? clientInputValue)
        (set! matrix1 clientInputValue)
        (defineMatrix)
        )
    )
  (define (recieveBoard)
    (let recieveBoardCicle ([previousBoard matrix1])
      (if (and (vector? clientInputValue) (not (equal? previousBoard clientInputValue)))
          (begin
            (set! matrix1 clientInputValue)
            (recieveBoardCicle matrix1)
            )
          (recieveBoardCicle previousBoard)
          )
      )
    )
  (define recieveBoardThread (thread recieveBoard))
  (define (sendBoard)
    (let sendBoardCicle ([previousBoard matrix2])
      (if (not (equal? previousBoard matrix2))
          (begin
            (clientOutput moves2)
            (sleep 0.25)
            (clientOutput matrix2)
            (sendBoardCicle matrix2)
            )
          (sendBoardCicle previousBoard)
          )
      )
    )
  (define sendBoardThread (thread sendBoard))
  (let playerDone? ()
    (if (pair? clientInputValue)
        (begin
          (thread-suspend inputThread)
          (set! score1 (car clientInputValue))
          (set! moves1 (cdr clientInputValue))
          (thread-resume inputThread)
          )
        (playerDone?)
        )
    )
  (set! player1done? #t)
  (newline)
  (display "player 1 done")
  (newline)
  (let waitGame ()
    (if gameDone?
        (clientOutput player1Result)
        (waitGame)
        )
    )
  (kill-thread inputThread)
  (kill-thread recieveBoardThread)
  (kill-thread sendBoardThread)
  )
(define server1Thread (thread server1))
(define (server2)
  (define listener (tcp-listen 2001))
  (define-values (in out) (tcp-accept listener))
  (define clientInputValue "")
  (define (clientInput)
    (let getInput
      ([input (read in)])
      (if (not (equal? input clientInputValue))
          (begin
            (set! clientInputValue input)
            (if (number? input)
                (set! moves2 input)
                )
            )
      )
      (getInput (read in))
    )
    )
  (define inputThread (thread clientInput))
  (define (clientOutput value)
    (write value out) (newline out)
    (flush-output out)
    )
  (clientOutput base)
  (let ready? ()
    (if (equal? clientInputValue "Base recieved")
        (set! player2Ready? #t)
        (ready?)
        )
    )
  (let oponentReady? ()
    (if player1Ready?
        (clientOutput "ready")
        (oponentReady?)
        )
    )
  (let defineMatrix ()
    (if (vector? clientInputValue)
        (set! matrix1 clientInputValue)
        (defineMatrix)
        )
    )
  (define (recieveBoard)
    (let recieveBoardCicle ([previousBoard matrix2])
      (if (and (vector? clientInputValue) (not (equal? previousBoard clientInputValue)))
          (begin
            (set! matrix2 clientInputValue)
            (recieveBoardCicle matrix2)
            )
          (recieveBoardCicle previousBoard)
          )
      )
    )
  (define recieveBoardThread (thread recieveBoard))
  (define (sendBoard)
    (let sendBoardCicle ([previousBoard matrix1])
      (if (not (equal? previousBoard matrix1))
          (begin
            (clientOutput moves1)
            (sleep 0.25)
            (clientOutput matrix1)
            (sendBoardCicle matrix1)
            )
          (sendBoardCicle previousBoard)
          )
      )
    )
  (define sendBoardThread (thread sendBoard))
  (let playerDone? ()
    (if (pair? clientInputValue)
        (begin
          (thread-suspend inputThread)
          (set! score2 (car clientInputValue))
          (set! moves2 (cdr clientInputValue))
          (thread-resume inputThread)
          )
        (playerDone?)
        )
    )
  (set! player2done? #t)
  (newline)
  (display "player 2 done")
  (newline)
  (let waitGame ()
    (if gameDone?
        (clientOutput player2Result)
        (waitGame)
        )
    )
  (kill-thread inputThread)
  (kill-thread recieveBoardThread)
  (kill-thread sendBoardThread)
  )
(define server2Thread (thread server2))
(define (compareResults)
  (cond
    [(< score1 score2)
     (set! player1Result "Loser")
     (set! player2Result "Winner")
     ]
    [(> score1 score2)
     (set! player1Result "Winner")
     (set! player2Result "Loser")
     ]
    [(= moves1 moves2)
     (cond
       [(< moves1 moves2)
        (set! player1Result "Winner")
        (set! player2Result "Loser")
        ]
       [(> moves1 moves2)
        (set! player1Result "Loser")
        (set! player2Result "Winner")
        ]
       [(= moves1 moves2)
        (set! player1Result "Tie")
        (set! player2Result "Tie")
        ]
       )
     ]
    )
  (set! gameDone? #t)
  )
(let endGame ()
  (if (and player1done? player2done?)
      (begin
        (newline)
        (display "comparando")
        (newline)
        (compareResults)
        )
      (endGame)
      )
  )
(sleep 10)
(kill-thread server1Thread)
(kill-thread server2Thread)
  