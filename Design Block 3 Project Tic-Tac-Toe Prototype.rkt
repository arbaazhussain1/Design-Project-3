#lang racket/gui

; o and x /Tic-Tac-Toe game
; With the GUI. with the code to attempts/endeavour to implement similar approach to Model View Controller (MVC). .

(define myFrame (new frame%
                     [label "The O and X Game"]
                     [width 300] [height 350]
                     ))


;---------------------------------------------------------------------------------------
; Model: State of the game
; use vectors as they are mutable - we can change the state
; The value of the "board" is a vector of "vector".
; This code is creating a representation of the game board being the o and x /Tic-Tac-Toe game.
(define board (vector
               (vector 0 0 0)
               (vector 0 0 0)
               (vector 0 0 0) ))

; Size of o and x /Tic-Tac-Toe game
; Also, the code is creating set boundaries,this being the Rows & Columns: 
(define rows 3)
(define cols 3)

; The current players turn to go: this being either 1 or 2 player
(define CurrentPlayer 1)

;---------------------------------------------------------------------
; functions to modify state

(define updateState (λ (rowNum colNum val)
                      ; update the state 
                      (vector-set! (vector-ref board rowNum) colNum val)
                      ))

(define SwitchPlayer (λ () (set! CurrentPlayer (- 3 CurrentPlayer))))


;----------------------------------------

;; Testing: vector-ref board. : rows, cols and dias.

#|
row1 (vector-ref board 2
row2 (vector-ref board 1
row3 (vector-ref board 0
    col1 (vector-ref (vector-ref board 2) 2)
         (vector-ref (vector-ref board 1) 2)
         (vector-ref (vector-ref board 0) 2)))
    col2 (vector-ref (vector-ref board 2) 1)
         (vector-ref (vector-ref board 1) 1)
         (vector-ref (vector-ref board 0) 1)))
    col3 (vector-ref (vector-ref board 2) 0)
         (vector-ref (vector-ref board 1) 0)
         (vector-ref (vector-ref board 0) 0)))
        dia1 (vector-ref (vector-ref board 2) 2)
             (vector-ref (vector-ref board 1) 1)
             (vector-ref (vector-ref board 0) 0)))
        dia2 (vector-ref (vector-ref board 2) 0)
             (vector-ref (vector-ref board 1) 1)
             (vector-ref (vector-ref board 0) 2)))
|#

(define non-zero? (λ (a)
                    (not (zero? a))))




(define winner (λ ()
                 (cond

                   
                   ;;THE PLAYER 1 POSSIBILITES OF  WINNING:
                   
                   ((equal? '(1 1 1) (vector->list (vector-ref board 2))) 1)
                   ((equal? '(1 1 1) (vector->list (vector-ref board 1))) 1)
                   ((equal? '(1 1 1) (vector->list (vector-ref board 0))) 1)
                   
                   ((equal? '(1 1 1) (list (vector-ref (vector-ref board 2) 2)
                                           (vector-ref (vector-ref board 1) 2)
                                           (vector-ref (vector-ref board 0) 2))) 1)
                   
                   ((equal? '(1 1 1) (list (vector-ref (vector-ref board 2) 1)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 1))) 1)
                   
                   ((equal? '(1 1 1) (list (vector-ref (vector-ref board 2) 0)
                                           (vector-ref (vector-ref board 1) 0)
                                           (vector-ref (vector-ref board 0) 0))) 1)
                   
                   ((equal? '(1 1 1) (list (vector-ref (vector-ref board 2) 2)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 0))) 1)
                   
                   ((equal? '(1 1 1) (list (vector-ref (vector-ref board 2) 0)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 2))) 1)


                   
                   ;;THE PLAYER 2 POSSIBILITES OF  WINNING:
                   
                   ((equal? '(2 2 2) (vector->list (vector-ref board 2))) 2)
                   ((equal? '(2 2 2) (vector->list (vector-ref board 1))) 2)
                   ((equal? '(2 2 2) (vector->list (vector-ref board 0))) 2)
                   
                   ((equal? '(2 2 2) (list (vector-ref (vector-ref board 2) 2)
                                           (vector-ref (vector-ref board 1) 2)
                                           (vector-ref (vector-ref board 0) 2))) 2)
                   
                   ((equal? '(2 2 2) (list (vector-ref (vector-ref board 2) 1)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 1))) 2)
                   
                   ((equal? '(2 2 2) (list (vector-ref (vector-ref board 2) 0)
                                           (vector-ref (vector-ref board 1) 0)
                                           (vector-ref (vector-ref board 0) 0))) 2)
                   
                   ((equal? '(2 2 2) (list (vector-ref (vector-ref board 2) 2)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 0))) 2)
                   
                   ((equal? '(2 2 2) (list (vector-ref (vector-ref board 2) 0)
                                           (vector-ref (vector-ref board 1) 1)
                                           (vector-ref (vector-ref board 0) 2))) 2)

                   
                   ;;THE POSSIBILITES OF AN DRAW:
                   
                   ((andmap equal? '(#t #t #t)
                           (list
                            (andmap non-zero? (vector->list (vector-ref board 0)))
                            (andmap non-zero? (vector->list (vector-ref board 1)))
                            (andmap non-zero? (vector->list (vector-ref board 2))))) 3)
                   (#t 0))))



; return winner (1 or 2) or 0 if no winner

; row
; col
; diagonal


;---------------------------------------------------------------------
; View: create button matrix;
(define Font1 (make-font #:size 46
                         #:face #f
                         #:family 'script
                         #:style 'normal
                         #:weight 'bold
                         
                         ))

; adds a button to container - callback does the actionFunc and calls update to refresh display
; updateFunc added to the updator set
(define AddButton (λ (container ActionFunction UpdateFunction rowNum colNum)
                    (let ((newBut
                           (new button% [label ""] [parent container]
                                [callback
                                   (λ (button event)
                                      (ActionFunction)
                                         (update)
                                                 )]
                                [font Font1] [min-width 100] [min-height 100])
                           ))
                      (AddUpdator (lambda () (UpdateFunction newBut)))
                      )
                    ))


(define CreateButtonRow (λ (RowContainer RowNumber NumberOfButtons)
                       ; create a row of buttons
                       (cond
                         ((equal? NumberOfButtons 0))
                         (else  (let ((buttonIndex (+ (* cols (- RowNumber 1)) NumberOfButtons)))
                                  
                                  (AddButton
                                   RowContainer
                                   (λ () ; actionFunc: set data item buttonIndex to currentPlayer; change currentPlayer
                                     (updateState (- RowNumber 1) (- NumberOfButtons 1) CurrentPlayer)
                                     (SwitchPlayer)
                                     )
                                   (λ (button) (UpdateButton button (- RowNumber 1) (- NumberOfButtons 1) )) ; updateFunc
                                   RowNumber NumberOfButtons)
                                  
                                  )
                                (CreateButtonRow RowContainer RowNumber (- NumberOfButtons 1)))
                         )))


(define CreateButtonRows (λ (vContainer rowCount colCount)
                           ; create a rowCount rows of buttons
                           (cond
                             ((equal? rowCount 0) )
                             (else  
                              (CreateButtonRow 
                               (new horizontal-panel% [parent vContainer]
                                    [alignment '(center center)])  rowCount colCount)
                              (CreateButtonRows vContainer (- rowCount 1) colCount) 
                              )
                             )
                           )
  )
(define CreateBoard (λ (container rowCount colCount) 
                      (CreateButtonRows 
                       (new vertical-panel% [parent container]
                            [alignment '(center center)])
                       rowCount colCount)
                      )
  )

; add a message box at the bottom: this will be updated by do show whose turn it is
(define Font2 (make-font #:size 20
                         #:face #f
                         #:family 'modern
                         #:style 'italic
                         #:weight'bold)) 

(define messageBox (new message% [label ""] [parent myFrame] [font Font2] [auto-resize #t]))

(define AddMessageArea (λ ()
                         
                         (AddUpdator (λ ()
                                       (let ((w (winner)))
                                         (cond
                                           ((equal? w 0) (send messageBox set-label (format "Player ~a to play" CurrentPlayer)))
                                           ((equal? w 3) (send messageBox set-label "It's a DRAW !!!"))
                                           (#t (send messageBox set-label (format "Winner is Player ~a !!!" w)))
                                           )
                                         ))
                                     )))

;---------------------------------------------------------------------
; update a button - change label and disable

(define UpdateButton (λ (but rowNumber colNumber)
                       (let* ((val (vector-ref (vector-ref board  rowNumber) colNumber))
                              (str (cond
                                     ((= val 1) "X")
                                     ((= val 2) "O")
                                     (#t "")
                                     )))
                         (send but set-label str)
                         (cond ((> val 0) (send but enable #f))
                               ((equal? (send messageBox get-label) "Winner is Player 1 !!!")
                                (send but enable #f))
                               ((equal? (send messageBox get-label) "Winner is Player 2 !!!")
                                (send but enable #f))
                               (#t (send but enable #t))
                               ; disable if already played in that cell
                               )
                         )
                       ))

; list of functions to update display
(define Updators (list))
; run all update functions
(define update (λ ()
                 (for-each (λ (func) (func))  Updators)
                 ))
(define AddUpdator (λ (fun)
                     (set! Updators (cons fun Updators))))


;---------------------------------------------------------------------
; initialisation - create the view

(CreateBoard myFrame rows cols)
(AddMessageArea)
(send myFrame show #t)

(define RestartGame (new button%
                    [parent myFrame]
                    [label "Restart"]
                    [callback (λ (o e)
                                (vector-set! board 0 (vector 0 0 0))
                                (vector-set! board 1 (vector 0 0 0))
                                (vector-set! board 2 (vector 0 0 0))
                                (set! CurrentPlayer 1)
                                (update)
                                )]))

(update)