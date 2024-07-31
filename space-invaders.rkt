;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 5)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
(define-struct tank (x dir))
(define-struct invader (x y dx))
(define-struct missile (x y))

;; Events

(define (tock ws)
  (cond [(< (length (game-invaders ws)) INVADE-RATE) (tock (make-game (create-invaders (game-invaders ws)) (game-missiles ws) (game-tank ws)))]
        [else (make-game
               (update-invaders (second (handle-collisions (game-missiles ws) (game-invaders ws))))
               (update-missiles (first (handle-collisions (game-missiles ws) (game-invaders ws))))
               (update-tank (game-tank ws)))]))

(define (handle-key ws key)
  (cond [(key=? key " ")
         (make-game
          (game-invaders ws)
          (append (list (make-missile (tank-x (game-tank ws)) (- HEIGHT 20))) (game-missiles ws))
          (game-tank ws))]
        [(key=? key "left")
         (make-game
          (game-invaders ws)
          (game-missiles ws)
          (make-tank (+ (tank-x (game-tank ws)) TANK-SPEED) -1))]
        [(key=? key "right")
         (make-game
          (game-invaders ws)
          (game-missiles ws)
          (make-tank (+ (tank-x (game-tank ws)) TANK-SPEED) 1))]
        [else ws]))

(define (landed? i)
  (cond [(empty? i) #f]
        [(>= (invader-y (first i)) HEIGHT) #t]
        [else (landed? (rest i))]))

(define (invader-landed? ws)
  (if (empty? (game-invaders ws))
      #f
      (landed? (game-invaders ws))))

(define (render-tank tank base-image)
  (place-image TANK (tank-x tank) (- HEIGHT 15) base-image))

(define (render-missiles missiles base-image)
  (cond
    [(empty? missiles) base-image]
    [else
     (render-missiles (rest missiles)
                      (place-image MISSILE
                                   (missile-x (first missiles))
                                   (missile-y (first missiles))
                                   base-image))]))
                                   
(define (render-invaders invaders base-image)
  (cond
    [(empty? invaders) base-image]
    [else
     (render-invaders (rest invaders)
                      (place-image INVADER
                                   (invader-x (first invaders))
                                   (invader-y (first invaders))
                                   base-image))]))

(define (render game)
  (render-tank (game-tank game)
               (render-missiles (game-missiles game)
                                (render-invaders (game-invaders game)
                                                 BACKGROUND))))
;; World Creation

(define (main ws)
  (big-bang ws
    (state true)
    (on-tick tock)
    (to-draw render)
    (on-key handle-key)
    (stop-when invader-landed?)))




;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define (update-tank t)
  (cond [(>= (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (- WIDTH TANK-HEIGHT/2)) (make-tank (+ (tank-x t) TANK-SPEED) -1)]
        [(<= (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) TANK-HEIGHT/2) (make-tank (+ (tank-x t) TANK-SPEED) 1)]
        [else (make-tank (+ (* (tank-dir t) TANK-SPEED) (tank-x t)) (tank-dir t))]))


;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

; Function to update the position of a single invader
(define (update-invader inv)
  (cond [(>= (+ (invader-x inv) (* (invader-dx inv) INVADER-X-SPEED)) (- WIDTH TANK-HEIGHT/2))
         (make-invader (+ (invader-x inv) (invader-dx inv))
                       (+ (invader-y inv) INVADER-Y-SPEED)
                       -1)]
        [(<= (+ (invader-x inv) (* (invader-dx inv) INVADER-X-SPEED)) TANK-HEIGHT/2)
         (make-invader
          (+ (invader-x inv) (invader-dx inv))
          (+ (invader-y inv) INVADER-Y-SPEED)
          1)]
        [else
         (make-invader
          (+ (invader-x inv) (* (invader-dx inv) INVADER-X-SPEED))
          (+ (invader-y inv) INVADER-Y-SPEED)
          (invader-dx inv))]))

(define (random-dx number)
  (if (zero? (random number))
      -1
      1))

; Function to update the positions of all invaders in a list
(define (update-invaders invaders)
  (cond
    [(empty? invaders) empty]
    [else (cons (update-invader (first invaders))
                (update-invaders (rest invaders)))]))

(define (create-invaders invaders)
  (cond [(< (length invaders) INVADE-RATE) (create-invaders (cons (make-invader (random WIDTH) 0 (random-dx 2)) invaders))]
        [else invaders]))



;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 0))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define (update-missiles m)
  (cond [(empty? m) empty]
        [else (cons (update-missiles-position (first m)) (update-missiles (rest m)))]))

(define (update-missiles-position m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

(define (in-range? value min max)
  (if (and (>= value min) (<= value max))
      #t
      #f))
 
(define (collide? m i)
  (if (and (in-range? (missile-y m) (- (invader-y i) HIT-RANGE) (+ (invader-y i) HIT-RANGE)) (in-range? (missile-x m) (- (invader-x i) HIT-RANGE) (+ (invader-x i) HIT-RANGE)))
      #t
      #f))

(define (offscreen-missile? m)
  (< (missile-y  m) 0))

(define (missile-collided? m i)
  (cond
    [(empty? i) #t]
    [(offscreen-missile? m) #f]
    [(collide?  m (first i)) #f]
    [else (missile-collided? m (rest i))]))

(define (handle-missile-colision m i)
  (cond
    [(empty? m) empty]
    [(missile-collided? (first m) i)
     (cons (first m) (handle-missile-colision (rest m) i))]
    [else (handle-missile-colision (rest m) i)]))

(define (invader-collided? m i)
  (cond
    [(empty? m) #t]
    [(collide?  (first m) i) #f]
    [else (invader-collided? (rest m) i)]))

(define (handle-invader-colision m i)
  (cond
    [(empty? i) empty]
    [(invader-collided? m (first i))
     (cons (first i) (handle-invader-colision m (rest i)))]
    [else (handle-invader-colision  m (rest i))]))

(define (handle-collisions m i)
  (cond
    [(and (empty? m) (empty? i)) (list empty empty)]
    [(empty? m) (list empty i)]
    [(empty? i) (list m empty)]
    [(collide? (first m) (first i))
     (handle-collisions (rest m) (rest i))]
    [else
     (list (handle-missile-colision m i) (handle-invader-colision m i))]))

;; World Creation Helpers

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))