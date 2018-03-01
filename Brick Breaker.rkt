;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |HW 6 full game DONE! (1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

#|--- DATA DEFINITIONS ---|#
;;A Ball is a (make-ball Number Number Number Number)
(define-struct ball (x y vx vy))
; - where the first Number is the ball's x-coordinate
; - the second Number is the ball's y-coordinate
; - the third Number is the ball's x-velocity
; - the fourth Number is the ball's y-velocity

;; the world
(define-struct level [paddle bricks ball score lives])
;; - paddle is a NatureNumber for the paddle's x value
;; - bricks is a list of posns
;; - ball is a ball
;; - points is a NaturalNumber
;; - lives is a NaturalNumber

#|--- CONSTANTS ---|#
(define WIDTH 200)
(define HEIGHT 200)
(define MIDDLEX (/ WIDTH 2))
(define MIDDLEY (/ HEIGHT 2))

(define GAMESCENE (rectangle WIDTH HEIGHT "solid" "white"))
(define TRANSPARENTSCENE (rectangle WIDTH HEIGHT "outline" "white"))

(define SCORELOC (make-posn (- WIDTH 30) (- HEIGHT 10)))
(define LIVESLOC (make-posn (- WIDTH 10) (- HEIGHT 10)))

(define BRICK-COLOR 'red)
(define BRICK-WIDTH 30)
(define BRICK-HEIGHT 10)
(define BRICK-PADDING 10)
(define ROWS 3)
(define COLUMNS 5)
(define TOTAL-BRICKS (* ROWS COLUMNS))
(define STARTING-BRICK-X (- MIDDLEX (/ (* (- COLUMNS 1) (+ BRICK-WIDTH BRICK-PADDING)) 2))) 
(define STARTING-BRICK-Y (/ BRICK-HEIGHT 2))
(define STARTING-BRICK (make-posn STARTING-BRICK-X STARTING-BRICK-Y))

(define PADDLE-COLOR 'purple)
(define PADDLE-WIDTH 40)
(define PADDLE-HEIGHT BRICK-HEIGHT)
(define PADDLE-Y (- HEIGHT (/ PADDLE-HEIGHT 2)))
(define PADDLE-SPEED 5)

(define BALL-COLOR 'blue)
(define BALL-RADIUS 6)
(define BALL-SPEED 4)
(define BALL-STARTING-Y (- HEIGHT PADDLE-HEIGHT BALL-RADIUS))
(define STARTING-VX 0)
(define STARTING-VY BALL-SPEED)

(define LEFT-BOUND (/ PADDLE-WIDTH 2))
(define RIGHT-BOUND (- WIDTH LEFT-BOUND))

(define INITIAL-BALL (make-ball (/ WIDTH 2)
                                (- HEIGHT PADDLE-HEIGHT (/ BALL-RADIUS 2))
                                BALL-SPEED 0))

;;---------------------FUNUCITONS USED IN COSTS--------------------------
;; returns a list of bricks for the game
;; empty -> [list-of posn]
(define (make-bricks lob)
  (cond
    [(empty? lob) (make-bricks (cons STARTING-BRICK lob))]
    
    [(= (+ (length lob) 1) TOTAL-BRICKS) (cons (make-brick-right (first lob)) lob)]
    
    [else (if (= (modulo (length lob) COLUMNS) 0) ;; drop down
              (make-bricks (cons (make-brick-down (first lob)) lob))
              (make-bricks (cons (make-brick-right (first lob)) lob)))]))

(check-expect (make-bricks '()) (list
                                 (make-posn 180 45)
                                 (make-posn 140 45)
                                 (make-posn 100 45)
                                 (make-posn 60 45)
                                 (make-posn 20 45)
                                 (make-posn 180 25)
                                 (make-posn 140 25)
                                 (make-posn 100 25)
                                 (make-posn 60 25)
                                 (make-posn 20 25)
                                 (make-posn 180 5)
                                 (make-posn 140 5)
                                 (make-posn 100 5)
                                 (make-posn 60 5)
                                 (make-posn 20 5)))



;; return a brick that is the right brick that is the first birck of a row
;; posn -> posn
(define (make-brick-down b)
  (make-posn STARTING-BRICK-X (+ (posn-y b) BRICK-HEIGHT BRICK-PADDING)))

(check-expect (make-brick-down (make-posn 8 8)) (make-posn STARTING-BRICK-X  28))
(check-expect (make-brick-down (make-posn 5 0)) (make-posn STARTING-BRICK-X  20))


;; returns a brick that is to the right of a given brick
;; posn -> posn
(define (make-brick-right b)
  (make-posn (+ (posn-x b) BRICK-WIDTH BRICK-PADDING) (posn-y b)))

(check-expect (make-brick-right (make-posn 0 0)) (make-posn 40 0))
(check-expect (make-brick-right (make-posn 6 3)) (make-posn 46 3))

#|--- DATA EXAMPLES ---|#

;; balls:
(define ball1 (make-ball 5 7 9 3))
(define ball2 (make-ball 4 0 20 20))
(define ball3 (make-ball 200 200 5 5))
(define ball4 (make-ball 40 40 7 78))
(define ball5 (make-ball 40 40 -3 -4))
(define ball6 (make-ball 30 30 -4 4))
(define starting-ball (make-ball MIDDLEX BALL-STARTING-Y 0 0))

;; bricks:
(define brick1 (make-posn 180 180))
(define brick2 (make-posn 180 160))
(define brick3 (make-posn 160 160))
(define brick4 (make-posn 160 180))
(define brick5 (make-posn 0 0))
(define brick6 (make-posn 8 4))

(define bricks1 (list brick1 brick2 brick3 brick4))
(define bricks2 (list brick1))
(define bricks3 (list brick1 brick2))
(define empty-bricks '())


;; for testing the side bounds of bricks
(define bricks11 (list (make-posn 61 45)(make-posn 61 35)(make-posn 62 45)(make-posn 62 35)))
(define bricks12 (list (make-posn 19 45)(make-posn 19 35)(make-posn 18 45)(make-posn 18 35))) 
(define bricks13 (list (make-posn 62 45)(make-posn 62 35)(make-posn 18 45)(make-posn 18 35)))

;; for testing the top and bottom bounds of bricks
(define bricks14 (list (make-posn 55 51) (make-posn 55 29) (make-posn 55 52) (make-posn 55 28)))
(define bricks15 (list (make-posn 25 29) (make-posn 25 51) (make-posn 25 28) (make-posn 25 52)))
(define bricks16 (list (make-posn 25 28) (make-posn 25 52) (make-posn 55 52) (make-posn 55 28)))

(define printed-bricks1
  (overlay  (place-image
             (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR) 180 180 TRANSPARENTSCENE)
                                   
            (place-image
             (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR) 180 160 TRANSPARENTSCENE)
                                   
            (place-image
             (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR) 160 180 TRANSPARENTSCENE)
                                   
            (place-image
             (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR) 160 160 TRANSPARENTSCENE)
                                   
            TRANSPARENTSCENE))

(define printed-bricks2
  (overlay  (place-image
             (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR) 180 180 TRANSPARENTSCENE)
                                  
            TRANSPARENTSCENE))

;; levels:
(define level1 (make-level 100 bricks1 ball1 4 6))
(define level2 (make-level RIGHT-BOUND bricks2 ball2 3 4))
(define level3 (make-level LEFT-BOUND empty-bricks ball3 0 0))
(define level11 (make-level 7 bricks1 ball2 0 13))
(define level12 (make-level LEFT-BOUND empty-bricks ball3 0 1))
(define level13 (make-level LEFT-BOUND bricks1 ball3 0 0))
(define level15 (make-level 90 '() (make-ball 6 6 8 8) 9 9))
(define level16 (make-level 0 '() (make-ball 0 6 8 8) 9 9))
(define level17 (make-level 0 '() (make-ball 200 100 5 5) 7 7))
(define level18 (make-level 5 '() (make-ball 30 0 8 8) 69 69))
(define level21 (make-level MIDDLEX (list (make-posn 19 40)) ball4 2 1))
(define level22 (make-level MIDDLEX (list (make-posn 200 200)) (make-ball 100 202 8 8) 2 1))
(define level23 (make-level MIDDLEX (list (make-posn 200 200)) (make-ball 100 202 8 8) 2 0))
(define level24 (make-level MIDDLEX (list (make-posn 200 200)) (make-ball 100 170 8 8) 2 1))
(define game (make-level MIDDLEX (make-bricks '()) starting-ball 0 5))
(define off-screen (make-level MIDDLEX bricks1 ball4 5 1))
(define hit2bricks (make-level MIDDLEX (list (make-posn 25 40) (make-posn 55 40)) ball4 5 1))
(define hitbrickvert (make-level MIDDLEX bricks1 (make-ball 180 180 9 9) 5 1))
(define ball-hit-wall (make-level MIDDLEX '() (make-ball 200 180 3 3) 4 4))

#|--- TEMPS ---|

;; a list of bricks is either:
;; - empty
;: - (cons posn list-of-bricks)
(define (brick-list-temp bl)
  (cond
    [(empty? (rest bl))...]
    [else ...(brick-list-temp(rest bl))]))

(define (ball-temp ball)
  ...(ball-x ball)
  ...(ball-y ball)
  ...(ball-vx ball)
  ...(ball-vy ball))

(define (level-temp lv)
  ...(level-paddle lv)
  ...(brick-list-temp (level-bricks lv))
  ...(level-ball lv)
  ...(level-score lv)
  ...(level-lives lv))
|#


#|--- FUNCTIONS ---|#
;; speed: Ball -> Number
;; compute the speed of the ball
(check-expect (speed INITIAL-BALL) 4)
(define (speed ball)
  (sqrt (+ (sqr (ball-vx ball))
           (sqr (ball-vy ball)))))

;;new-x-velocity : Ball Number -> Number
;;Produces the new x velocity of a ball that launched off a paddle with this x-coordinate
(define (new-x-velocity ball x)
  (inexact->exact
   (* .95
      (/ (- (ball-x ball) x) (+ (/ PADDLE-WIDTH 2) BALL-RADIUS))
      (speed ball))))
(check-expect (new-x-velocity INITIAL-BALL 100) 0)
(check-expect (new-x-velocity (make-ball 60 190 3 4) 100)
              (inexact->exact (* 4.75 -40/26)))

;;new-y-velocity : Ball Number -> Number
;;Produces the new y velocity of a ball that launched off a paddle with this x-coordinate
(define (new-y-velocity ball x)
  (inexact->exact
   (* (- (sqrt (- 1 (sqr (* .95
                            (/ (- (ball-x ball) x) (+ (/ PADDLE-WIDTH 2) BALL-RADIUS)))))))
      (speed ball))))
(check-expect (new-y-velocity INITIAL-BALL 100) -4)
(check-expect (new-y-velocity (make-ball 60 190 3 4) 100)
              (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26)))))))

;;launch-ball : Ball Number -> Ball
;;Launch ball off paddle with this x-coordinate
(define (launch-ball ball x)
  (make-ball (+ (ball-x ball) (new-x-velocity ball x))
             (+ (ball-y ball) (new-y-velocity ball x))
             (new-x-velocity ball x) (new-y-velocity ball x)))
(check-expect (launch-ball INITIAL-BALL 100)
              (make-ball 100 183 0 -4))
(check-expect (launch-ball (make-ball 60 190 3 4) 100)
              (make-ball (+ 60 (inexact->exact (* 4.75 -40/26)))
                         (+ 190 (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26)))))))
                         (inexact->exact (* 4.75 -40/26))
                         (inexact->exact (* -5 (sqrt (- 1 (sqr (* .95 -40/26))))))))

;; sets off the ball when teh player hit the spacebar
;; ball NAturalNumber -> ball
(define (set-off-ball b x)
  (launch-ball (make-ball (ball-x b) (ball-y b) STARTING-VX STARTING-VY) x))

(check-expect (set-off-ball ball1 30) (launch-ball (make-ball 5 7 STARTING-VX STARTING-VY) 30))
(check-expect (set-off-ball ball2 20) (launch-ball (make-ball 4 0 STARTING-VX STARTING-VY) 20))


;; renders the given world
;; level -> image
(define (draw-level lv)
  (overlay (draw-paddle (level-paddle lv))
           (draw-lob (level-bricks lv))
           (draw-ball (level-ball lv))
           (draw-score (level-score lv))
           (draw-lives (level-lives lv))
           (draw-message lv)))


(check-expect (draw-level level1)
              (overlay (place-image
                        (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" PADDLE-COLOR)
                        100 PADDLE-Y TRANSPARENTSCENE)
                       
                       printed-bricks1
                                          
                       (place-image
                        (circle BALL-RADIUS "solid" BALL-COLOR)
                        5 7 TRANSPARENTSCENE)
                                          
                       (place-image
                        (text "4" 14 "black") (posn-x SCORELOC) (posn-y SCORELOC) TRANSPARENTSCENE)
                                          
                       (place-image
                        (text "6" 14 "black") (posn-x LIVESLOC) (posn-y LIVESLOC) TRANSPARENTSCENE)))

(check-expect (draw-level level2)
              (overlay (place-image
                        (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" PADDLE-COLOR)
                        RIGHT-BOUND PADDLE-Y TRANSPARENTSCENE)
                       printed-bricks2
                                          
                       (place-image
                        (circle BALL-RADIUS "solid" BALL-COLOR)
                        4 0 TRANSPARENTSCENE)
                                          
                       (place-image
                        (text "3" 14 "black") (posn-x SCORELOC)
                        (posn-y SCORELOC) TRANSPARENTSCENE)
                                          
                       (place-image
                        (text "4" 14 "black") (posn-x LIVESLOC)
                        (posn-y LIVESLOC) TRANSPARENTSCENE)))

(check-expect (draw-level level3)
              (overlay (place-image
                        (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" PADDLE-COLOR)
                        LEFT-BOUND PADDLE-Y TRANSPARENTSCENE)
                       TRANSPARENTSCENE
                                          
                       (place-image
                        (circle BALL-RADIUS "solid" BALL-COLOR)
                        200 200 TRANSPARENTSCENE)
                                          
                       (place-image
                        (text "0" 14 "black")
                        (posn-x SCORELOC) (posn-y SCORELOC) TRANSPARENTSCENE)
                                          
                       (place-image
                        (text "0" 14 "black")
                        (posn-x LIVESLOC) (posn-y LIVESLOC) TRANSPARENTSCENE)

                       (place-image
                        (text "YOU WIN!" 9 "black")
                        MIDDLEX MIDDLEY TRANSPARENTSCENE)))


;; draws a message to the streen
;; level -> image
(define (draw-message lv)
  (cond
    [(empty? (level-bricks lv))
     (place-image
      (text "YOU WIN!" 9 "black") MIDDLEX MIDDLEY TRANSPARENTSCENE)]
    [(= (level-lives lv) 0)
     (place-image
      (text "GAME OVER!" 9 "black") MIDDLEX MIDDLEY TRANSPARENTSCENE)]
    [(ball-not-moving (level-ball lv))
     (place-image
      (text "SRESS SPACE TO LAUNCH THE BALL" 9 "black") MIDDLEX MIDDLEY TRANSPARENTSCENE)]
    [else TRANSPARENTSCENE]))

(check-expect (draw-message game) (place-image
                                   (text "SRESS SPACE TO LAUNCH THE BALL" 9 "black")
                                   MIDDLEX MIDDLEY TRANSPARENTSCENE))

(check-expect (draw-message level3) (place-image
                                     (text "YOU WIN!" 9 "black")
                                     MIDDLEX MIDDLEY TRANSPARENTSCENE))

(check-expect (draw-message level13) (place-image
                                      (text "GAME OVER!" 9 "black")
                                      MIDDLEX MIDDLEY TRANSPARENTSCENE))

(check-expect (draw-message level11) TRANSPARENTSCENE)

(check-expect (draw-message level12) (place-image
                                      (text "YOU WIN!" 9 "black")
                                      MIDDLEX MIDDLEY TRANSPARENTSCENE))

;; renders the paddle at a given location
;; posn -> image
(define (draw-paddle paddle)
  (place-image
   (rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" PADDLE-COLOR) paddle PADDLE-Y TRANSPARENTSCENE))

(check-expect (draw-paddle 0) (place-image(rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" PADDLE-COLOR)
                                          0 PADDLE-Y TRANSPARENTSCENE))

(check-expect (draw-paddle 200)(place-image(rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" PADDLE-COLOR)
                                           200 PADDLE-Y TRANSPARENTSCENE))

(check-expect (draw-paddle 69)(place-image(rectangle PADDLE-WIDTH PADDLE-HEIGHT "solid" PADDLE-COLOR)
                                          69 PADDLE-Y TRANSPARENTSCENE))

;; renders the list of bricks 
;; [list-of posn] (lob) -> image
(define (draw-lob lob)
  (cond
    [(empty? lob) TRANSPARENTSCENE]
    [else (overlay (draw-brick (first lob)) (draw-lob(rest lob)))]))

(check-expect (draw-lob bricks1) printed-bricks1)
(check-expect (draw-lob bricks2) printed-bricks2)
(check-expect (draw-lob empty-bricks) TRANSPARENTSCENE)


;; render a singular brick at a given location
;; posn -> image
(define (draw-brick b)
  (place-image
   (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR) (posn-x b) (posn-y b) TRANSPARENTSCENE))

(check-expect (draw-brick (make-posn 0 0))
              (place-image
               (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR) 0 0 TRANSPARENTSCENE))

(check-expect (draw-brick (make-posn 200 200))
              (place-image
               (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR) 200 200 TRANSPARENTSCENE))

(check-expect (draw-brick (make-posn 22 22))
              (place-image
               (rectangle BRICK-WIDTH BRICK-HEIGHT "solid" BRICK-COLOR) 22 22 TRANSPARENTSCENE))

;; renders the ball at a given posn
;; posn -> image
(define (draw-ball b)
  (place-image
   (circle BALL-RADIUS "solid" BALL-COLOR) (ball-x b) (ball-y b) TRANSPARENTSCENE))

(check-expect (draw-ball ball1) (place-image
                                 (circle BALL-RADIUS "solid" BALL-COLOR) 5 7 TRANSPARENTSCENE))

(check-expect (draw-ball ball2) (place-image
                                 (circle BALL-RADIUS "solid" BALL-COLOR) 4 0 TRANSPARENTSCENE))

(check-expect (draw-ball ball3) (place-image
                                 (circle BALL-RADIUS "solid" BALL-COLOR) 200 200 TRANSPARENTSCENE))

;; renders the score to the screen
;; NaturalNumber -> image
(define (draw-score score)
  (place-image
   (text (number->string score) 14 "black") (posn-x SCORELOC) (posn-y SCORELOC) TRANSPARENTSCENE))

(check-expect (draw-score 69) (place-image
                               (text "69" 14 "black")
                               (posn-x SCORELOC) (posn-y SCORELOC) TRANSPARENTSCENE))

(check-expect (draw-score 0) (place-image
                              (text "0" 14 "black")
                              (posn-x SCORELOC) (posn-y SCORELOC) TRANSPARENTSCENE))

(check-expect (draw-score 2) (place-image
                              (text "2" 14 "black")
                              (posn-x SCORELOC) (posn-y SCORELOC) TRANSPARENTSCENE))

;; renders the number of lives to the screen
;; NaturalNumber -> image
(define (draw-lives lives)
  (place-image
   (text (number->string lives) 14 "black") (posn-x LIVESLOC) (posn-y LIVESLOC) TRANSPARENTSCENE))

(check-expect (draw-lives 4) (place-image
                              (text "4" 14 "black")
                              (posn-x LIVESLOC) (posn-y LIVESLOC) TRANSPARENTSCENE))

(check-expect (draw-lives 0) (place-image
                              (text "0" 14 "black")
                              (posn-x LIVESLOC) (posn-y LIVESLOC) TRANSPARENTSCENE))

(check-expect (draw-lives 77) (place-image
                               (text "77" 14 "black")
                               (posn-x LIVESLOC) (posn-y LIVESLOC) TRANSPARENTSCENE))


;; Moves the paddle left when the player presses the left arrow key and vise versa for the right key
;; It does this while making sure that the paddle stays within the determined bounds of the scene
;; level key-event -> level
(define (move-paddle lv key)
  (cond
    [(key=? key "left") (on-key-left lv)]
    
    [(key=? key "right")  (on-key-right lv)]

    [(and (key=? key " ") (ball-not-moving (level-ball lv))) (make-level
                                                              (level-paddle lv)
                                                              (level-bricks lv)
                                                              (set-off-ball (level-ball lv)
                                                                            (level-paddle lv))
                                                              (level-score lv)
                                                              (level-lives lv))]
    [else lv]))

;; updates the world when the right key in pressed
;; level -> level
(define (on-key-right lv)
  (if (ball-not-moving (level-ball lv))
      (make-level
       (move-paddle-right (level-paddle lv))
       (level-bricks lv)
       (make-ball (move-paddle-right (level-paddle lv)) (ball-y (level-ball lv))
                  (ball-vx (level-ball lv)) (ball-vy (level-ball lv)))
       (level-score lv)
       (level-lives lv))

      (make-level
       (move-paddle-right (level-paddle lv))
       (level-bricks lv)
       (level-ball lv)
       (level-score lv)
       (level-lives lv))))

(check-expect (on-key-right (make-level
                             MIDDLEX
                             bricks1
                             starting-ball
                             0
                             0)) (make-level
                                  (move-paddle-right MIDDLEX)
                                  bricks1
                                  (make-ball (move-paddle-right MIDDLEX) BALL-STARTING-Y 0 0)
                                  0
                                  0))

(check-expect (on-key-right (make-level
                             0
                             bricks1
                             ball1
                             0
                             0)) (make-level
                                  5
                                  bricks1
                                  ball1
                                  0
                                  0))

(check-expect (on-key-right (make-level
                             RIGHT-BOUND
                             bricks1
                             ball5
                             0
                             0)) (make-level
                                  RIGHT-BOUND
                                  bricks1
                                  ball5
                                  0
                                  0))

(check-expect (move-paddle (make-level
                            RIGHT-BOUND
                            bricks1
                            ball1
                            0
                            0) "left") (make-level
                                        (- RIGHT-BOUND 5)
                                        bricks1
                                        ball1
                                        0
                                        0))

(check-expect (move-paddle (make-level
                            (+ LEFT-BOUND 6)
                            bricks1
                            ball1
                            0
                            0) "left") (make-level
                                        (+ LEFT-BOUND 1)
                                        bricks1
                                        ball1
                                        0
                                        0))

;; updates the world when the right key in pressed
;; level -> level
(define (on-key-left lv)
  (if (ball-not-moving (level-ball lv))
                        
      (make-level
       (move-paddle-left (level-paddle lv))
       (level-bricks lv)
       (make-ball (move-paddle-left (level-paddle lv)) (ball-y (level-ball lv))
                  (ball-vx (level-ball lv)) (ball-vy (level-ball lv)))
       (level-score lv)
       (level-lives lv))
                        
      (make-level
       (move-paddle-left (level-paddle lv))
       (level-bricks lv)
       (level-ball lv)
       (level-score lv)
       (level-lives lv))))

(check-expect (on-key-left (make-level
                            MIDDLEX
                            bricks1
                            starting-ball
                            0
                            0)) (make-level
                                 (move-paddle-left MIDDLEX)
                                 bricks1
                                 (make-ball (move-paddle-left MIDDLEX) BALL-STARTING-Y 0 0)
                                 0
                                 0))

(check-expect (on-key-left (make-level
                            LEFT-BOUND
                            bricks1
                            ball1
                            0
                            0)) (make-level
                                 LEFT-BOUND
                                 bricks1
                                 ball1
                                 0
                                 0))

(check-expect (on-key-left (make-level
                            (+ LEFT-BOUND 4)
                            bricks1
                            ball5
                            0
                            0)) (make-level
                                 LEFT-BOUND
                                 bricks1
                                 ball5
                                 0
                                 0))
  

;; return if the ball is moving or not
;; ball -> boolean
(define (ball-not-moving ball)
  (and (= (ball-vx ball) 0) (= (ball-vy ball) 0)))

(check-expect (ball-not-moving ball1) #false)
(check-expect (ball-not-moving ball2) #false)
(check-expect (ball-not-moving starting-ball) #true)


(check-expect (move-paddle (make-level
                            7
                            bricks1
                            ball1
                            0
                            0) " ") (make-level
                                     7
                                     bricks1
                                     ball1
                                     0
                                     0))

(check-expect (move-paddle (make-level
                            7
                            bricks1
                            ball2
                            0
                            0) " ") (make-level
                                     7
                                     bricks1
                                     ball2
                                     0
                                     0))

(check-expect (move-paddle (make-level
                            7
                            bricks1
                            starting-ball
                            0
                            0) " ") (make-level
                                     7
                                     bricks1
                                     (set-off-ball starting-ball 7)
                                     0
                                     0))

(check-expect (move-paddle (make-level
                            (+ LEFT-BOUND 4)
                            bricks1
                            ball1
                            0
                            0) "left") (make-level
                                        LEFT-BOUND
                                        bricks1
                                        ball1
                                        0
                                        0))

(check-expect (move-paddle (make-level
                            RIGHT-BOUND
                            bricks1
                            ball1
                            0
                            0) "left") (make-level
                                        (- RIGHT-BOUND 5)
                                        bricks1
                                        ball1
                                        0
                                        0))

(check-expect (move-paddle (make-level
                            (+ LEFT-BOUND 6)
                            bricks1
                            ball1
                            0
                            0) "left") (make-level
                                        (+ LEFT-BOUND 1)
                                        bricks1
                                        ball1
                                        0
                                        0))
(check-expect (move-paddle (make-level
                            LEFT-BOUND
                            bricks1
                            ball1
                            0
                            0) "left") (make-level
                                        LEFT-BOUND
                                        bricks1
                                        ball1
                                        0
                                        0))

(check-expect (move-paddle (make-level
                            (- RIGHT-BOUND 4)
                            bricks1
                            ball1
                            0
                            0) "right") (make-level
                                         RIGHT-BOUND
                                         bricks1
                                         ball1
                                         0
                                         0))

(check-expect (move-paddle (make-level
                            0
                            bricks1
                            ball1
                            0
                            0) "right") (make-level
                                         5
                                         bricks1
                                         ball1
                                         0
                                         0))

(check-expect (move-paddle (make-level
                            RIGHT-BOUND
                            bricks1
                            ball1
                            0
                            0) "right") (make-level
                                         RIGHT-BOUND
                                         bricks1
                                         ball1
                                         0
                                         0))

(check-expect (move-paddle (make-level
                            (- RIGHT-BOUND 6)
                            bricks1
                            ball1
                            0
                            0) "right") (make-level
                                         (- RIGHT-BOUND 1)
                                         bricks1
                                         ball1
                                         0
                                         0)) 


(check-expect (move-paddle (make-level
                            0
                            bricks1
                            ball1
                            0
                            0) "up") (make-level
                                      0
                                      bricks1
                                      ball1
                                      0
                                      0))


;; Moves the paddle to the left and check the bounds.
;; If the paddle reaches the bound, it cannot move any further left
;; NaturalNumber -> NaturalNumber
(define (move-paddle-left paddle)
  (if (> (- paddle PADDLE-SPEED) LEFT-BOUND)
      (- paddle PADDLE-SPEED)
      LEFT-BOUND))

(check-expect (move-paddle-left (+ LEFT-BOUND 4)) LEFT-BOUND)
(check-expect (move-paddle-left RIGHT-BOUND) (- RIGHT-BOUND 5))
(check-expect (move-paddle-left LEFT-BOUND) LEFT-BOUND)
(check-expect (move-paddle-left (+ LEFT-BOUND 6)) (+ LEFT-BOUND 1))

;; Moves the paddle to the right and check the bounds.
;; If the paddle reaches the bound, it cannot move any further right
;; NaturalNumber -> NaturalNumber
(define (move-paddle-right paddle)
  (if (< (+ paddle PADDLE-SPEED) RIGHT-BOUND)
      (+ paddle PADDLE-SPEED)
      RIGHT-BOUND))

(check-expect (move-paddle-right (- RIGHT-BOUND 4)) RIGHT-BOUND)
(check-expect (move-paddle-right 0) 5)
(check-expect (move-paddle-right RIGHT-BOUND) RIGHT-BOUND)
(check-expect (move-paddle-right (- RIGHT-BOUND 6)) (- RIGHT-BOUND 1))


;; update all vars in the game
;; level -> level
(define (update lv)
  (cond
    [(and (ball-hit-wall-side? (level-ball lv))
          (or (ball-hit-wall-top? (level-ball lv))
              (ball-hit-paddle? (level-ball lv) (level-paddle lv))))
     (on-ball-hit-corner lv)]
    
    [(ball-hit-paddle? (level-ball lv) (level-paddle lv))
     (on-ball-hit-paddle lv)]
    
    [(ball-hit-bricks-ver? (level-bricks lv) (level-ball lv))
     (on-hit-bricks-ver lv)]

    [(ball-hit-bricks-side? (level-bricks lv) (level-ball lv))
     (on-ball-hit-brick-side lv)]
    
    [(ball-hit-wall-side? (level-ball lv))
     (on-ball-hit-wall-side lv)]

    [(ball-hit-wall-top? (level-ball lv))
     (on-ball-hit-wall-top lv)]
    
    [(and (ball-hit-ground? (level-ball lv))
          (> (level-lives lv) 0))
     (on-ball-hit-ground lv)]
    
    [else (on-ball-unimpeded lv)]))


;; edge cases covered is helper check-expects
(check-expect (update game) (on-ball-hit-corner game))
(check-expect (update level1) (on-ball-hit-brick-side level1))
(check-expect (update hitbrickvert) (on-hit-bricks-ver hitbrickvert))
(check-expect (update ball-hit-wall) (on-ball-hit-wall-side ball-hit-wall))
(check-expect (update level15) (on-ball-hit-corner level15))
(check-expect (update level16) (on-ball-hit-corner level16))
(check-expect (update level18) (on-ball-hit-wall-top level18))
(check-expect (update level21) (on-ball-hit-brick-side level21))
(check-expect (update off-screen) (on-ball-unimpeded off-screen))
(check-expect (update level22) (on-ball-hit-ground level22))
(check-expect (update level23) (on-ball-unimpeded level23))
(check-expect (update level24) (on-ball-unimpeded level24))





              

;; when the ball hits a corner
;; level -> level
(define (on-ball-hit-corner lv)
  (make-level
   (level-paddle lv)
   (level-bricks lv)
   (invert-xy (level-ball lv))
   (level-score lv)
   (level-lives lv)))

(check-expect (on-ball-hit-corner off-screen)
              (make-level
               MIDDLEX
               bricks1
               (invert-xy ball4)
               5
               1))
              
  

;; when the ball his the paddle
;; level -> level
(define (on-ball-hit-paddle lv)
  (make-level
   (level-paddle lv)
   (level-bricks lv)
   (launch-ball (level-ball lv) (level-paddle lv))
   (level-score lv)
   (level-lives lv)))

(check-expect (on-ball-hit-paddle off-screen)
              (make-level
               MIDDLEX
               bricks1
               (launch-ball ball4 MIDDLEX)
               5
               1))
              
  
;; when the ball his the bricks on the top or bottom
;; level -> level
(define (on-hit-bricks-ver lv)
  (make-level
   (level-paddle lv)
   (del-bricks (level-bricks lv) (bricks-hit-ball (level-bricks lv) (level-ball lv)))
   (invert-y (level-ball lv))
   (+ (level-score lv) (length (bricks-hit-ball (level-bricks lv) (level-ball lv))))
   (level-lives lv)))

(check-expect (on-hit-bricks-ver hitbrickvert)
              (make-level
               MIDDLEX
               (list brick2 brick3)
               (make-ball 189 171 9 -9)
               7
               1))
             

;; when the ball his the brick side
;; level -> level
(define (on-ball-hit-brick-side lv)
  (if (>= (length (bricks-hit-ball (level-bricks lv) (level-ball lv))) 2)
      (make-level
       (level-paddle lv)
       (del-bricks (level-bricks lv) (bricks-hit-ball (level-bricks lv) (level-ball lv)))
       (invert-y (level-ball lv))
       (+ (level-score lv) (length (bricks-hit-ball (level-bricks lv) (level-ball lv))))
       (level-lives lv))
      (make-level
       (level-paddle lv)
       (del-bricks (level-bricks lv) (bricks-hit-ball (level-bricks lv) (level-ball lv)))
       (invert-x (level-ball lv))
       (+ (level-score lv) (length (bricks-hit-ball (level-bricks lv) (level-ball lv))))
       (level-lives lv))))

(check-expect (on-ball-hit-brick-side hit2bricks)
              (make-level
               MIDDLEX
               '()
               (make-ball 47 -38 7 -78)
               7
               1))

(check-expect (on-ball-hit-brick-side off-screen)
              (make-level
               MIDDLEX
               bricks1
               (make-ball 33 118 -7 78)
               5
               1))

(check-expect (on-ball-hit-brick-side off-screen)
              (make-level
               MIDDLEX
               bricks1
               (make-ball 33 118 -7 78)
               5
               1))
               

;; when the ball hits the side of the wall
;; level -> level
(define (on-ball-hit-wall-side lv)
  (make-level
   (level-paddle lv)
   (level-bricks lv)
   (invert-x (level-ball lv))
   (level-score lv)
   (level-lives lv)))

(check-expect (on-ball-hit-wall-side off-screen)
              (make-level
               MIDDLEX
               bricks1
               (make-ball 33 118 -7 78)  
               5
               1))
  
;; when the ball hit the top wall
;; level -> level
(define (on-ball-hit-wall-top lv)
  (make-level
   (level-paddle lv)
   (level-bricks lv)
   (invert-y (level-ball lv))
   (level-score lv)
   (level-lives lv)))

(check-expect (on-ball-hit-wall-top off-screen)
              (make-level
               MIDDLEX
               bricks1
               (make-ball 47 -38 7 -78)  
               5
               1))
                                                 
        
  

;; when the ball hits the ground
;; level -> level
(define (on-ball-hit-ground lv)
  (if (= (level-lives lv) 1)
      (make-level
       (level-paddle lv)
       (level-bricks lv)
       (level-ball lv)
       (- (level-score lv) 1)
       (- (level-lives lv) 1))
      (make-level
       MIDDLEX
       (level-bricks lv)
       starting-ball
       (- (level-score lv) 1)
       (- (level-lives lv) 1))))

(check-expect (on-ball-hit-ground game)
              (make-level
               MIDDLEX
               (make-bricks '())
               starting-ball
               -1
               4))

(check-expect (on-ball-hit-ground off-screen)
              (make-level
               MIDDLEX
               bricks1
               ball4
               4
               0))

;; when the ball in not hitting anything
;; level -> level
(define (on-ball-unimpeded lv)
  (make-level
   (level-paddle lv)
   (level-bricks lv)
   (move-ball (level-ball lv))
   (level-score lv)
   (level-lives lv)))

(check-expect (on-ball-unimpeded game)
              (make-level
               MIDDLEX
               (make-bricks '())
               (move-ball starting-ball)
               0
               5))
              

;; inverts the x velocity of the ball
;; ball -> ball
(define (invert-x ball)
  (make-ball (+ (ball-x ball) (* -1 (ball-vx ball)))
             (+ (ball-y ball) (ball-vy ball))
             (* -1 (ball-vx ball))
             (ball-vy ball)))

(check-expect (invert-x ball1) (make-ball -4 10 -9 3))
(check-expect (invert-x ball2) (make-ball -16 20 -20 20))
(check-expect (invert-x ball3) (make-ball 195 205 -5 5))

;; inverts the y velocity of the ball
;; ball -> ball
(define (invert-y ball)
  (make-ball (+ (ball-x ball) (ball-vx ball))
             (+ (ball-y ball) (* -1 (ball-vy ball)))
             (ball-vx ball)
             (* -1 (ball-vy ball))))

(check-expect (invert-y ball1) (make-ball 14 4 9 -3))
(check-expect (invert-y ball2) (make-ball 24 -20 20 -20))
(check-expect (invert-y ball3) (make-ball 205 195 5 -5))

;; inverts both the x and y velocity of the ball
;; ball -> ball
(define (invert-xy ball)
  (make-ball (+ (ball-x ball) (* -1 (ball-vx ball)))
             (+ (ball-y ball) (* -1 (ball-vy ball)))
             (* -1 (ball-vx ball))
             (* -1 (ball-vy ball))))

(check-expect (invert-xy ball1) (make-ball -4 4 -9 -3))
(check-expect (invert-xy ball2) (make-ball -16 -20 -20 -20))
(check-expect (invert-xy ball3) (make-ball 195 195 -5 -5))

;; deletes the bricks from the first list that are shared with the second
;; note: the bricks given will always be in the list
;; list of posn list of posn -> list of posn
(define (del-bricks lob lob2)
  (cond
    [(empty? lob) lob]
    [(contains? lob2 (first lob)) (del-bricks (rest lob) lob2)]
    [else (cons (first lob) (del-bricks (rest lob) lob2))]))

(check-expect (del-bricks bricks1 bricks1) '())
(check-expect (del-bricks bricks1 bricks2) (list brick2 brick3 brick4))
(check-expect (del-bricks bricks1 bricks3) (list brick3 brick4))
(check-expect (del-bricks bricks2 empty-bricks) bricks2)

;; list of posn posn -> list of posn
(define (contains? lob b)
  (cond
    [(empty? lob) #false]
    [(brick-equal? (first lob) b) #true]
    [else (contains? (rest lob) b)]))

(check-expect (contains? bricks1 brick1) #true)
(check-expect (contains? bricks1 brick2) #true)
(check-expect (contains? bricks1 brick5) #false)
(check-expect (contains? bricks1 brick6) #false)


;; return if two bricks are equal
;; posn posn -> boolean
(define (brick-equal? b1 b2)
  (and (= (posn-x b1) (posn-x b2))
       (= (posn-y b1) (posn-y b2))))

(check-expect (brick-equal? (make-posn 2 2) (make-posn 2 2)) #true)
(check-expect (brick-equal? (make-posn 69 3) (make-posn 69 3)) #true)
(check-expect (brick-equal? (make-posn 1 1) (make-posn 2 2)) #false)
(check-expect (brick-equal? (make-posn 1 1) (make-posn 8 8)) #false)

;; return if the ball is in the paddle
;; ball NaturalNumber -> boolean
(define (ball-hit-paddle? b paddle)
  (and (>= (+ (ball-y b) BALL-RADIUS) (- HEIGHT PADDLE-HEIGHT))
       (< (+ (ball-y b) BALL-RADIUS) HEIGHT)
       (<= (- (ball-x b) BALL-RADIUS) (+ paddle (/ PADDLE-WIDTH 2)))
       (>= (+ (ball-x b) BALL-RADIUS) (- paddle (/ PADDLE-WIDTH 2)))))

(check-expect (ball-hit-paddle? (make-ball 66 190 0 0) 40) #true)
(check-expect (ball-hit-paddle? (make-ball 14 192 0 0) 40) #true)
(check-expect (ball-hit-paddle? (make-ball 14 20 0 0) 40) #false)
(check-expect (ball-hit-paddle? (make-ball 80 190 0 0) 40) #false)
(check-expect (ball-hit-paddle? (make-ball 66 200 0 0) 40) #false)

;; return a list of bricks that the ball hit and it's loc
;; [list-of posn] ball -> [list-of posn]
(define (bricks-hit-ball lob ball)
  (cond
    [(empty? lob) lob]
    [else (if (ball-hit-brick? (first lob) ball)
              (cons (first lob) (bricks-hit-ball (rest lob) ball))
              (bricks-hit-ball (rest lob) ball))]))

(check-expect (bricks-hit-ball bricks1 (make-ball 186 174 0 0)) bricks1)
(check-expect (bricks-hit-ball bricks1 (make-ball 154 154 0 0)) (list brick2 brick3))
(check-expect (bricks-hit-ball bricks1 (make-ball 2 2 2 2)) '())
(check-expect (bricks-hit-ball bricks2 (make-ball 154 186 0 0)) bricks2)
(check-expect (bricks-hit-ball '() ball1) '())

;; determines if the ball has hit the side or top/bottom of a brick
;; [list-of posn] ball -> boolean
(define (ball-hit-brick? brick ball)
  (and (<= (- (ball-x ball) BALL-RADIUS) (+ (posn-x brick) BRICK-WIDTH))
       (>= (+ (ball-x ball) BALL-RADIUS) (- (posn-x brick) BRICK-WIDTH))
       (<= (- (ball-y ball) BALL-RADIUS) (+ (posn-y brick) BRICK-HEIGHT))
       (>= (+ (ball-y ball) BALL-RADIUS) (- (posn-y brick) BRICK-HEIGHT))))

(check-expect (ball-hit-brick? (make-posn 76 56) ball4) #true)
(check-expect (ball-hit-brick? (make-posn 76 24) ball4) #true)
(check-expect (ball-hit-brick? (make-posn 4 24) ball4) #true)
(check-expect (ball-hit-brick? (make-posn 4 56) ball4) #true)
(check-expect (ball-hit-brick? (make-posn 77 57) ball4) #false)
(check-expect (ball-hit-brick? (make-posn 77 23) ball4) #false)
(check-expect (ball-hit-brick? (make-posn 3 23) ball4) #false)
(check-expect (ball-hit-brick? (make-posn 3 57) ball4) #false)

;; return if the ball it hitting the brick on the sides of the brick
;; posn ball -> boolean
(define (ball-hit-bricks-side? bricks ball)
  (cond
    [(empty? bricks) #false]
    [(hit-side? (first bricks) ball) #true]
    [else (ball-hit-bricks-side? (rest bricks) ball)]))

(check-expect (ball-hit-bricks-side? bricks11 ball4) #true)
(check-expect (ball-hit-bricks-side? bricks12 ball4) #true)
(check-expect (ball-hit-bricks-side? bricks13 ball4) #false)

;; return if the ball it hitting the top/bottom of the brick
;; posn ball -> boolean
(define (ball-hit-bricks-ver? bricks ball)
  (cond
    [(empty? bricks) #false]
    [(hit-topbottom? (first bricks) ball) #true]
    [else (ball-hit-bricks-ver? (rest bricks) ball)]))

(check-expect (ball-hit-bricks-ver? bricks14 ball4) #true)
(check-expect (ball-hit-bricks-ver? bricks15 ball4) #true)
(check-expect (ball-hit-bricks-ver? bricks16 ball4) #false)

;; determines if the ball hits the top/bottom of a brick
;; brick ball -> boolean
(define (hit-topbottom? brick ball)
  (and (>= (+ (posn-y brick) BALL-RADIUS (/ BRICK-HEIGHT 2)) (ball-y ball))
       (<= (- (posn-y brick) BALL-RADIUS (/ BRICK-HEIGHT 2)) (ball-y ball))
       (>= (+ (posn-x brick) (/ BRICK-WIDTH 2)) (ball-x ball))
       (<= (- (posn-x brick) (/ BRICK-WIDTH 2)) (ball-x ball))))


(check-expect (hit-topbottom? (make-posn 55 51) ball4) #true)
(check-expect (hit-topbottom? (make-posn 55 29) ball4) #true)
(check-expect (hit-topbottom? (make-posn 25 29) ball4) #true)
(check-expect (hit-topbottom? (make-posn 25 51) ball4) #true)
(check-expect (hit-topbottom? (make-posn 55 52) ball4) #false)
(check-expect (hit-topbottom? (make-posn 55 28) ball4) #false)
(check-expect (hit-topbottom? (make-posn 25 28) ball4) #false)
(check-expect (hit-topbottom? (make-posn 25 52) ball4) #false)

;; determines if the ball hits the side of a brick
;; brick ball -> boolean
(define (hit-side? brick ball)
  (and (>= (+ (posn-y brick) (/ BRICK-HEIGHT 2)) (ball-y ball))
       (<= (- (posn-y brick) (/ BRICK-HEIGHT 2)) (ball-y ball))
       (>= (+ (posn-x brick) BALL-RADIUS (/ BRICK-WIDTH 2)) (ball-x ball))
       (<= (- (posn-x brick) BALL-RADIUS (/ BRICK-WIDTH 2)) (ball-x ball))))


(check-expect (hit-side? (make-posn 61 45) ball4) #true)
(check-expect (hit-side? (make-posn 61 35) ball4) #true)
(check-expect (hit-side? (make-posn 19 35) ball4) #true)
(check-expect (hit-side? (make-posn 19 45) ball4) #true)
(check-expect (hit-side? (make-posn 62 45) ball4) #false)
(check-expect (hit-side? (make-posn 62 35) ball4) #false)
(check-expect (hit-side? (make-posn 18 35) ball4) #false)
(check-expect (hit-side? (make-posn 18 45) ball4) #false)

;; return if the ball in hiting the side wall of the game
;; ball -> boolean
(define (ball-hit-wall-side? b)
  (or (>= (+ (ball-x b) BALL-RADIUS) WIDTH)
      (<= (- (ball-x b) BALL-RADIUS) 0)))

(check-expect (ball-hit-wall-side? (make-ball 194 6 0 0)) #true)
(check-expect (ball-hit-wall-side? (make-ball 194 194 0 0)) #true)
(check-expect (ball-hit-wall-side? (make-ball 193 7 0 0)) #false)
(check-expect (ball-hit-wall-side? (make-ball 193 193 0 0)) #false)

;; return if the ball in hiting the top wall of the game
;; ball -> boolean
(define (ball-hit-wall-top? b)
  (<= (- (ball-y b) BALL-RADIUS) 0))

(check-expect (ball-hit-wall-top? (make-ball 6 6 0 0)) #true)
(check-expect (ball-hit-wall-top? (make-ball 7 7 0 0)) #false)

;; return if the ball it the ground
;; ball -> boolean
(define (ball-hit-ground? b)
  (>= (+ (ball-y b) BALL-RADIUS) HEIGHT))

(check-expect (ball-hit-ground? (make-ball 7 193 0 1)) #false)
(check-expect (ball-hit-ground? (make-ball 6 193 1 0)) #false)
(check-expect (ball-hit-ground? (make-ball 6 194 0 0)) #true)
(check-expect (ball-hit-ground? (make-ball 2 194 0 0)) #true)


;; update the ball's locaiton when it does not hit an object
;; ball -> ball
(define (move-ball b)
  (make-ball (+ (ball-x b) (ball-vx b))
             (+ (ball-y b) (ball-vy b))
             (ball-vx b) (ball-vy b)))

(check-expect (move-ball ball1) (make-ball 14 10 9 3))
(check-expect (move-ball ball2) (make-ball 24 20 20 20))
(check-expect (move-ball ball5) (make-ball 37 36 -3 -4))
(check-expect (move-ball ball6) (make-ball 26 34 -4 4))

;; builds a world with the inputed number of lives
;; number -> level
(define (new-world n)
  (make-level (level-paddle game) (level-bricks game) (level-ball game) 0 n))

(check-expect (new-world 6) (make-level MIDDLEX (level-bricks game) starting-ball 0 6))

;; Brick Breaker
;; level -> level
(define (main n)
  (big-bang (new-world n)

    ;;level -> level
    (on-tick update)

    ;; level -> image
    (to-draw draw-level)
 
    ;; level keyEvent -> level
    (on-key move-paddle)))
    
