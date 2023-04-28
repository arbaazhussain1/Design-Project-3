#lang racket/gui



(require json)
(require rebellion/collection/table)
(require racket/gui)
(require racket/gui/base)

(define scoreboard '())

(define number-to-guess (random 100))

(define hacks (println number-to-guess))

(define number-of-guesses 0)
(define frame (new frame% [label "Number Guessing Game"]
                          [width 300]
                          [height 150]))
(define name-text (new text-field% [parent frame]
                               [label "Name"]
                               ))

(define entry (new text-field% [parent frame]
                               [label "Guess:"]
                               ))



(new button% [parent frame]
              [label "Guess"]
              [callback (lambda (button event)
                     (let ([guess (string->number (send entry get-value))])
                       (set! number-of-guesses (add1 number-of-guesses))
                       (cond
                         [(= guess number-to-guess)
                          (update-scoreboard (send name-text get-value) number-of-guesses )
                          (message-box "Correct!"
                                       (format "Congratulations , you guessed the number in ~a guesses!" number-of-guesses))]
                         [( > guess number-to-guess)
                          (message-box "Too High" "Your guess is too high, try again")]
                         [( < guess number-to-guess)
                          (message-box "Too Low" "Your guess is too low , try again")])))])
                          
              



(define (reset-game)
  (set! number-to-guess (random 100))
  (set! number-of-guesses 0)
  (send entry set-value ""))

(new button% [parent frame ]
             [label "Reset" ]
             [callback (lambda (button event)
                         (reset-game)
                         (message-box "Game reset" "A number has been generated. Good Luck!"))])




(define scoreboardframe (new frame% [label "Scoreboard"]
                            [width 400]
                            [height 400]
                            ))
(define scoreboardframe2 (new frame% [label "scoreboard"] [width 300] [height 200]))

(new button% [parent frame ]
     [label "Scoreboard" ]
     [callback (lambda (button event)
                 (send scoreboardframe2 show #t)
                 (println scoreboard)
                 )])




(send frame show #t)


 

(send frame show #t)











(define scoreboard2 (list (vector "Alex" 3 ) (vector "Bob" 4)))



(define find-name (λ (player-name SB) 
  (cond
    ((empty? SB) #f)
    ((equal? player-name (vector-ref (first SB) 0)) (first SB))
    (else (find-name player-name (rest SB))))))

 
(define update-scoreboard (λ (player-name attempts )
                            (define player-information (vector player-name attempts))
                            (set! scoreboard (cons player-information scoreboard))
                            ))

; for example: add some players to the scoreboard
;(update-scoreboard "Alex" 3)
;(update-scoreboard "Bob" 4)
;(update-scoreboard "Karl" 5)
;(update-scoreboard "Arbaaz" 2)

(displayln scoreboard) ; Output: ((vector "Arbaaz" 2) (vector "Karl" 5) (vector "Bob" 4) (vector "Alex" 3))
                            
  