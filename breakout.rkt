#lang racket
(require racket/gui)

;;; STILL A WORK IN PROGRESS

;;;
;;; BREAKOUT
;;;

; Jens Axel Søgaard, Feb 2014
;     https://github.com/soegaard/breakout


;;; Data Representation
(struct world      (bat bricks balls)    #:transparent)
(struct body       (x y w h)             #:transparent)
(struct brick body (strength)            #:transparent)
(struct ball  body (vx vy)               #:transparent)
(struct bat   body (dead?)               #:transparent)

;;; Configuration
(define width       400)
(define height      400)
(define bat-width    30)
(define bat-height    6)
(define ball-size     3)
(define brick-width  30)
(define brick-height 10)

;;; Smart Constructors

(define (new-ball x y vx vy)
  (ball x y ball-size ball-size vx vy))

(define (new-brick x y)
  (brick x y brick-width brick-height 1))

(define (new-bat x y)
  (bat x y bat-width bat-height #f))

;;;
;;; MODEL
;;;

;;; Creation

; create-world : -> world
;  the initial world contains a bat and a bunch of bricks
(define (create-world)
  (world (create-bat) (create-bricks) (list (create-ball))))

; create-bricks : -> (list body)
;   create list of twenty-four bricks
(define (create-bricks)
  (define w brick-width)
  (define h brick-height)
  (define gap 2)
  (define margin 30)
  (define rows 5)
  (define cols 10)
  (for/list ([i (* rows cols)])
    (define x (+ margin (* (+ w gap) (remainder i cols)))) 
    (define y (+ margin (* (+ h gap) (quotient  i cols)))) 
    (new-brick x y)))

; create-bat : -> bat
(define (create-bat)
  (define x (- (/ width 2.) (/ bat-width 2.)))
  (define y (- height (* 2. bat-height)))
  (new-bat x y))

(define (create-ball)
  (define x (- (/ width 2.) (/ bat-width 2.)))
  (define y (- height (* 2. bat-height)))
  (new-ball x y 0.3 -3))
  


;;; Updaters

; update-brick : brick -> brick
(define (update-brick b)
  (match-define (brick x y w h s) b)
  (brick x y w h s))

; update-ball : ball -> ball
(define (update-ball b)
  (match-define (ball x y w h vx vy) b)
  (ball (+ x vx) (+ y vy) w h vx vy))

; update-bat : world -> world
(define (update-bat W)
  (match-define (world b bricks balls) W)
  (match-define (bat x y w h dead?) b)
  (define moved-bat
    (cond [dead?              b]
          [(key-down? 'left)  (bat (- x 2.) y w h dead?)]
          [(key-down? 'right) (bat (+ x 2.) y w h dead?)]
          [else               (bat x y w h dead?)]))
  (world moved-bat bricks balls))

;;; UPDATES

(define (update w)
  (restart-on-r
   (handle-ball/brick-collisions
    (update-bat
     (update-bricks
      (update-balls w))))))


(define (update-bricks w)
  (define bs (world-bricks w))
  (struct-copy world w [bricks (map update-brick bs)]))

(define (update-balls w)
  (struct-copy world w [balls (map update-ball (world-balls w))]))

(define (restart-on-r w)
  (if (key-down? #\r)
      (create-world)
      w))

;;; Collision

(define (colliding? b1 b2)
  (match-define (body x1 y1 w1 h1) b1)
  (match-define (body x2 y2 w2 h2) b2)
  (not (or (eq? b1 b2)
           (< (+ x1 w1) x2) (> x1 (+ x2 w2))
           (< (+ y1 h1) y2) (> y1 (+ y2 h2)))))

(define (collisions? x bs)
  (for/or ([b bs]) (colliding? x b)))

(define (inside-screen? b)
  (match-define (body x y w h) b)
  (and (< 0 x width)
       (< 0 y height)))

(define (maybe-flip a-ball a-brick)
  (displayln (list a-ball a-brick))
  ; a collision between the ball and the body has been detected,
  ; maybe flip the x and y velocities of the ball
  (match-define (body bx by bw bh) a-brick)
  (define (maybe-flip-vx a-ball)
    (match-define (ball  x  y  w  h vx vy) a-ball)
    ; hit vertical side?
    (if (or (= (+ x w) bx) (= x (+ bx bw)))
        (ball x y w h (- vx) vy)
        a-ball))
  (define (maybe-flip-vy a-ball)
    (match-define (ball  x  y  w  h vx vy) a-ball)
    ; hit horizontal side?
    (if (or (= (+ y h) by) (= y (+ by bh)))
        (ball x y w h vx (- vy))
        a-ball))
  
  (maybe-flip-vy (maybe-flip-vx a-ball)))

(define (handle-ball/brick-collisions w)
  (match-define (world bat bricks balls) w)
  (define-values (new-bricks new-balls)
    (for/fold ([new-bricks '()] [new-balls '()]) ([ball balls])
      (for/fold ([new-bricks '()] [new-balls '()]) ([brick bricks])
        (if (colliding? brick ball)
            (values new-bricks (cons (maybe-flip ball brick) new-balls))
            (values (cons brick new-bricks) (cons ball new-balls))))))
  (world bat new-bricks new-balls))

;;; DRAWING

; draw-bodies : (list body) drawing-context -> void
;   draw the bodies in the world w to the drawing context dc
(define (draw-bodies bs dc)
  (for ([b bs])
    (match-define (body x y w h) b)
    (define c (if (bat? b) (if (bat-dead? b) "red" "green") "black"))
    (send dc set-brush (new brush% [color c] [style 'solid]))
    (send dc draw-rectangle x y w h)))

(define (draw-world w dc)
  (match-define (world bat bricks balls) w)
  (draw-bodies (append (list bat) bricks balls) dc))

;;; GUI STATE

(define the-world (create-world))

;;; Keyboard
; The keyboard state is kept in a hash table. 
; Use key-down? to find out, whether a key is pressed or not.
(define the-keyboard (make-hasheq))
(define (key-down! k) (hash-set! the-keyboard k #t))
(define (key-up! k)   (hash-set! the-keyboard k #f))
(define (key-down? k) (hash-ref  the-keyboard k #f))

;;; Canvas
; Key events sent to the canvas updates the information in the-keyboard.
; Paint events calls draw-world. To prevent flicker we suspend flushing
; while drawing commences.
(define game-canvas%
  (class canvas%
    (define/override (on-event e) ; mouse events
      'ignore)
    (define/override (on-char e)  ; key event
      (define key     (send e get-key-code))
      (define release (send e get-key-release-code))
      (when (eq? release 'press)  ; key down?
        (key-down! key))
      (when (eq? key 'release)    ; key up?
        (key-up! release)))
    (define/override (on-paint)   ; repaint (exposed or resized)
      (define dc (send this get-dc))
      (send this suspend-flush)
      (send dc clear)
      (draw-world the-world dc)
      (send this resume-flush))
    (super-new)))

; Create frame with canvas and show it.
(define frame  (new frame%  [label "Arkanoid"]))
(define canvas (new game-canvas% [parent frame] [min-width width] [min-height height]))
(send frame show #t)

; Start a timer. Each time the timer triggers, the world is updated.
(define timer (new timer% 
                   [notify-callback 
                    (λ () 
                      (set! the-world (update the-world))
                      (send canvas on-paint))]
                   [interval 100])) ; in milliseconds
(send timer start 20)


;;;
;;; INSTRUCTIONS
;;;

(displayln "Breakout")
(displayln "Move:  left and right arrow")
(displayln "Shoot: space")
(displayln "Reset: r")
