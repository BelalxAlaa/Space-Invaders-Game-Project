;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-SPEED 1)  ;speeds (not velocities) in pixels per tick
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

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

(define GAME-OVER (overlay (rotate 60(text "GAME OVER" 75 "red"))
                           (rectangle WIDTH HEIGHT "solid" "black")))





;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (ListOfInvader) (ListOfMissile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game g)
  (... (fn-for-loi (game-invaders g))
       (fn-for-lom (game-missiles g))
       (fn-for-tank (game-tank g))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;; - empty
;; - (cons invader ListOfInvader)

;; Interp. a list of invader

(define loi1 empty)
(define loi2 (list I1))
(define loi3 (list I1 I2))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else         
         (... (fn-for-invader (first loi))   ;invader
              (fn-for-loi(rest loi)))]))     ;ListOfInvader


;; Template rules used:
;; - one of: 2 cases
;; - atomic-distinct : empty
;; - compound: (cons invader ListOfInvader)
;; - reference: (first loi) is invader
;; - self-reference: (rest loi) is ListOfInvader



(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; - empty
;; - (cons missile ListOfMissile)

;; Interp. a list of missile

(define lom1 empty)
(define lom2 (list M1))
(define lom3 (list M1 M2))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom(rest lom)))]))


;; Template rules used:
;; - one of: 2 cases
;; - atomic-distinct : empty
;; - compound: (cons missile ListOfMissile)
;; - reference: (first lom) is missile
;; - self-reference: (rest lom) is ListOfMissile




(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; =================
;; Functions:

;; game -> game
;; start the world with tank with a BACKGROUND
;; 
(define (main g)
  (big-bang g                             ; game
    (on-tick   tock )                     ; game -> game
    (to-draw   render-game)               ; game -> Image
    (stop-when game-over last-picture)    ; game -> Boolean                        
    (on-key    handle-key)))              ; game KeyEvent -> game 

;; game -> game
;; produce the next 
;; !!!

(check-expect (tock
               (make-game empty empty (make-tank (/ WIDTH 2) 1)))
              (make-game (list (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED))
                         empty
                         (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))) 

(check-expect (tock
               (make-game (list (make-invader 150 100 INVADER-SPEED)) 
                          (list (make-missile 150 300))
                          (make-tank 50 1)))
              (make-game (list 
                          (make-invader (+ 150 INVADER-SPEED) (+ 100 INVADER-SPEED) INVADER-SPEED)
                          (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED))
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))
                         (make-tank (+ 50 TANK-SPEED) 1)))


                          



;; (define (tock game) G0)
;<template due to function composition>

(define (tock g)
  (move (hit g)))

;; game -> game
;; produces neat and tidy game

(check-expect (move
               (make-game (list (make-invader 100 150 INVADER-SPEED))
                          (list (make-missile 40 80))
                          T0))
              (make-game (invade (list (make-invader 100 150 INVADER-SPEED)))
                         (move-lom (list (make-missile 40 80)))
                         (move-tank T0)))

(check-expect (move
               (make-game (list (make-invader 100 150 INVADER-SPEED)
                                (make-invader 250 250 INVADER-SPEED))
                          (list (make-missile 40 80)
                                (make-missile 220 220))
                          T1))
              (make-game (invade (list (make-invader 100 150 INVADER-SPEED)
                                       (make-invader 250 250 INVADER-SPEED)))
                         (move-lom (list (make-missile 40 80)
                                         (make-missile 220 220)))
                         (move-tank T1)))
                                

;(define (move g) G0) ;stub
;<use template from game>
(define (move g)
  (make-game (invade (game-invaders g))
             (move-lom (game-missiles g))
             (move-tank (game-tank g))))


  

;; Tank -> Tank
;; produces the next tank by adding TANK-SPEED to the x

(check-expect (move-tank
               (make-tank (/ WIDTH 2) 1))
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))

(check-expect (move-tank
               (make-tank 50 1))
              (make-tank (+ 50 TANK-SPEED) 1))

(check-expect (move-tank
               (make-tank 50 -1))
              (make-tank (- 50 TANK-SPEED) -1))

(check-expect (move-tank
               (make-tank (- WIDTH (/ (image-width TANK) 2)) 1))
              (make-tank (- WIDTH (/ (image-width TANK) 2)) -1))

(check-expect (move-tank
               (make-tank (/ (image-width TANK) 2) -1))
              (make-tank (/ (image-width TANK) 2) 1))

;(define (move-tank t) t) ;stub

;<use template from Tank>
(define (move-tank t)
  (cond [(or (>= (tank-x t) (- WIDTH (/ (image-width TANK) 2)))
             (<= (tank-x t) (/ (image-width TANK) 2)))
         (make-tank (tank-x t) (- (tank-dir t)))]
        [else
         (if (= (tank-dir t) 1)
             (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))
             (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t)))])) 
             
      
;; ListOfMissile -> ListOfMissile
;; produces neat and tidy ListOfMissile

(check-expect (move-lom empty) empty)

(check-expect (move-lom
               (list (make-missile 100 150)))
              (list (make-missile 100 (- 150 MISSILE-SPEED))))

(check-expect (move-lom
               (list (make-missile 100 150)
                     (make-missile 200 250)))                           
              (list (make-missile 100 (- 150 MISSILE-SPEED))
                    (make-missile 200 (- 250 MISSILE-SPEED))))                          

(check-expect (move-lom
               (list (make-missile 100 (- (/ 2 (image-height MISSILE))))))                     
              empty)

(check-expect (move-lom
               (list (make-missile 100 (- (/ 2 (image-height MISSILE))))
                     (make-missile 100 150)))
              (list (make-missile 100 (- 150 MISSILE-SPEED))))                    

(check-expect (move-lom
               (list (make-missile 100 150)
                     (make-missile 100 (- (/ 2 (image-height MISSILE))))))                           
              (list (make-missile 100 (- 150 MISSILE-SPEED))))                    
                     
;(define (move-lom lom) empty) ;stub
;<use template from ListOfMissile>

(define (move-lom lom)
  (cond [(empty? lom) empty]
        [else
         (if (> (missile-y (first lom)) (/ 2 (image-height MISSILE)))
             (append (list (make-missile (missile-x (first lom)) (- (missile-y (first lom)) MISSILE-SPEED)))
                     (move-lom (rest lom)))
             (move-lom (rest lom)))]))

;; ListOfInvader -> ListOfInvader
;; produces neat and tidy list of invaders

(check-expect (invade empty)
              (list (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED)))

(check-expect (invade
               (list (make-invader 100 200 INVADER-SPEED)))
              (list 
               (make-invader (+ 100 INVADER-SPEED) (+ 200 INVADER-SPEED) INVADER-SPEED)
               (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED)))

(check-expect (invade
               (list (make-invader 100 200 INVADER-SPEED)
                     (make-invader 30 40 INVADER-SPEED)))
              (list 
               (make-invader (+ 100 INVADER-SPEED) (+ 200 INVADER-SPEED) INVADER-SPEED)
               (make-invader (+ 30 INVADER-SPEED) (+ 40 INVADER-SPEED) INVADER-SPEED)
               (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED)))

(check-expect (invade
               (list (make-invader 100 200 (- INVADER-SPEED))))
              (list 
               (make-invader (- 100 INVADER-SPEED) (+ 200 INVADER-SPEED) (- INVADER-SPEED))
               (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED)))

(check-expect (invade
               (list (make-invader 100 200 (- INVADER-SPEED))
                     (make-invader 30 40 (- INVADER-SPEED))))
              (list 
               (make-invader (- 100 INVADER-SPEED) (+ 200 INVADER-SPEED) (- INVADER-SPEED))
               (make-invader (- 30 INVADER-SPEED) (+ 40 INVADER-SPEED) (- INVADER-SPEED))
               (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED)))

(check-expect (invade
               (list (make-invader 100 200  INVADER-SPEED)
                     (make-invader 30 40 (- INVADER-SPEED))))
              (list 
               (make-invader (+ 100 INVADER-SPEED) (+ 200 INVADER-SPEED)  INVADER-SPEED)
               (make-invader (- 30 INVADER-SPEED) (+ 40 INVADER-SPEED) (- INVADER-SPEED))
               (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED)))

(check-expect (invade
               (list (make-invader (- WIDTH (/ (image-width INVADER) 2)) 200 INVADER-SPEED)))
              (list 
               (make-invader (- WIDTH (/ (image-width INVADER) 2)) 200 (- INVADER-SPEED))
               (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED)))

(check-expect (invade
               (list (make-invader  (/ (image-width INVADER) 2) 200 (- INVADER-SPEED))))
              (list 
               (make-invader  (/ (image-width INVADER) 2) 200  INVADER-SPEED)
               (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED)))

(check-expect (invade
               (list (make-invader (- WIDTH (/ (image-width INVADER) 2)) 200 INVADER-SPEED)
                     (make-invader  (/ (image-width INVADER) 2) 200 (- INVADER-SPEED))))
              (list 
               (make-invader (- WIDTH (/ (image-width INVADER) 2)) 200 (- INVADER-SPEED))
               (make-invader  (/ (image-width INVADER) 2) 200  INVADER-SPEED)
               (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED)))



;(define (invade loi) empty) ;stub
;<use template from ListOfInvader>

(define (invade loi)
  (cond [(empty? loi)
         (if (<= (random 3000) INVADE-RATE)
             (list (make-invader (random (- WIDTH (/ (image-width INVADER) 2))) 0 INVADER-SPEED))
             loi)]
        [else
         (append (list (cond [(or (>= (invader-x (first loi)) (- WIDTH (/ (image-width INVADER) 2)))
                                  (<= (invader-x (first loi)) (/ (image-width INVADER) 2)))
                              (bounce-invader (first loi))]                                    
                             [else
                              (if (positive? (invader-dx (first loi)))
                                  (make-invader (+ INVADER-SPEED (invader-x (first loi)))
                                                (+ INVADER-SPEED (invader-y (first loi)))
                                                (invader-dx (first loi)))
                                  (make-invader (- (invader-x (first loi)) INVADER-SPEED)
                                                (+ INVADER-SPEED (invader-y (first loi)))
                                                (invader-dx (first loi))))]))
                 (invade (rest loi)))]))


;; Invader -> Invader
;; moves the invader when it hits a wall

(check-expect (bounce-invader
               (make-invader (- WIDTH (/ (image-width INVADER) 2)) 50 INVADER-SPEED))
              (make-invader (- (- WIDTH (/ (image-width INVADER) 2)) INVADER-SPEED) (+ 50 INVADER-SPEED) (- INVADER-SPEED)))

(check-expect (bounce-invader
               (make-invader (/ (image-width INVADER) 2) 50 (- INVADER-SPEED)))
              (make-invader (+ (/ (image-width INVADER) 2) INVADER-SPEED) (+ 50 INVADER-SPEED) INVADER-SPEED))


;(define (bounce-invader i) i) ;stub
;<use template from invader>

(define (bounce-invader i)
  (if (= (invader-x i) (- WIDTH (/ (image-width INVADER) 2)))
      (make-invader (- (invader-x i) INVADER-SPEED) (+ (invader-y i) INVADER-SPEED) (- (invader-dx i)))
      (make-invader (+ (invader-x i) INVADER-SPEED) (+ (invader-y i) INVADER-SPEED) (- (invader-dx i)))))
                                 
;; game -> game
;; produce neat and tidy list of invaders and missiles , that means there is no hitten missiles or invaders in it

; both lists are empty
(check-expect (hit (make-game empty empty T0))
              (make-game empty empty T0))

; one list only empty  (LOI)
(check-expect (hit (make-game empty (list M1) T0))
              (make-game empty (list M1) T0))

; one list only empty (LOM)
(check-expect (hit (make-game (list I1) empty T0))
              (make-game (list I1) empty T0))

; one long list for both (not hit each other)
(check-expect (hit (make-game (list I1) (list M1) T0))
              (make-game (list I1) (list M1) T0))

; one long list for both (but hit each other)
(check-expect (hit (make-game
                    (list (make-invader 150 100 INVADER-SPEED))
                    (list (make-missile 150 105 ))
                    T0))
              (make-game empty empty T0))

; two long list for both (not hit each other)
(check-expect (hit (make-game
                    (list
                     (make-invader 100 150 INVADER-SPEED)
                     (make-invader 40 80 INVADER-SPEED))
                    (list
                     (make-missile 100 170)
                     (make-missile 40 100))
                    T0))
              (make-game
               (list
                (make-invader 100 150 INVADER-SPEED)
                (make-invader 40 80 INVADER-SPEED))
               (list
                (make-missile 100 170)
                (make-missile 40 100))
               T0))

; two long list (first invader hits first missile only)
(check-expect (hit (make-game
                    (list
                     (make-invader 100 150 INVADER-SPEED)
                     (make-invader 40 80 INVADER-SPEED))
                    (list
                     (make-missile 100 155)
                     (make-missile 40 100))
                    T0))
              (make-game
               (list                      
                (make-invader 40 80 INVADER-SPEED))
               (list                      
                (make-missile 40 100))
               T0))

;two long list (first invader hits second missile only)
(check-expect (hit (make-game
                    (list
                     (make-invader 100 150 INVADER-SPEED)
                     (make-invader 40 80 INVADER-SPEED))
                    (list
                     (make-missile 100 170)
                     (make-missile 100 155))
                    T0))
              (make-game
               (list                      
                (make-invader 40 80 INVADER-SPEED))
               (list
                (make-missile 100 170))                      
               T0))

;two long list (2nd invader his 1st missile)
(check-expect (hit (make-game
                    (list
                     (make-invader 100 150 INVADER-SPEED)
                     (make-invader 40 80 INVADER-SPEED))
                    (list
                     (make-missile 40 85)
                     (make-missile 40 100))
                    T0))
              (make-game
               (list
                (make-invader 100 150 INVADER-SPEED))                      
               (list                      
                (make-missile 40 100))
               T0))

;two long list (2nd invader hits second missile)
(check-expect (hit (make-game
                    (list
                     (make-invader 100 150 INVADER-SPEED)
                     (make-invader 40 80 INVADER-SPEED))
                    (list
                     (make-missile 100 170)
                     (make-missile 40 85))
                    T0))
              (make-game
               (list
                (make-invader 100 150 INVADER-SPEED))
               (list
                (make-missile 100 170))
               T0))

;two long list (1st invader hits 1st missile and 2nd invader hits 2nd missile)
(check-expect (hit (make-game
                    (list
                     (make-invader 100 150 INVADER-SPEED)
                     (make-invader 40 80 INVADER-SPEED))
                    (list
                     (make-missile 100 155)
                     (make-missile 40 85))
                    T0))
              (make-game
               empty
               empty
               T0))

;two long list (1st invader hits 2nd missile and 2nd invader hits 1st missile)
(check-expect (hit (make-game
                    (list
                     (make-invader 100 150 INVADER-SPEED)
                     (make-invader 40 80 INVADER-SPEED))
                    (list
                     (make-missile 40 85)
                     (make-missile 100 155))
                    T0))
              (make-game
               empty
               empty
               T0)) 
 
;(define (hit g) G0) ;stub

(define (hit g)
  (make-game (compare-loi (game-invaders g) (game-missiles g))
             (compare-lom (game-invaders g) (game-missiles g))
             (game-tank g)))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; produces neat and tidy list of invaders

(check-expect (compare-loi empty empty)
              empty)

(check-expect (compare-loi (list I1)
                           (list M1))
              (list I1))

(check-expect (compare-loi (list (make-invader 100 150 INVADER-SPEED))
                           (list (make-missile 100 155)))
              empty)

(check-expect (compare-loi (list (make-invader 100 150 INVADER-SPEED)
                                 (make-invader 120 170 INVADER-SPEED))
                           (list (make-missile 100 155)
                                 (make-missile 120 175)))
              empty)

(check-expect (compare-loi (list (make-invader 100 150 INVADER-SPEED)
                                 (make-invader 120 170 INVADER-SPEED))
                           (list (make-missile 100 155)
                                 (make-missile 40 80)))
              (list (make-invader 120 170 INVADER-SPEED)))

(check-expect (compare-loi (list (make-invader 100 150 INVADER-SPEED)
                                 (make-invader 120 170 INVADER-SPEED))
                           (list (make-missile 40 80)
                                 (make-missile 100 155)))
              (list (make-invader 120 170 INVADER-SPEED)))

;(define (compare-loi loi lom) loi) ;stub
(define (compare-loi loi lom)
  (cond [(empty? lom) loi]
        [else
         (compare-loi (compare-missile (first lom) loi) (rest lom))]))


;; Missile ListOfInvader -> ListOfInvader
;; remove the hit Invader from the ListOfInvader

(check-expect (compare-missile M1 empty)
              empty)

(check-expect (compare-missile (make-missile 100 150 )
                               (list (make-invader 100 150 INVADER-SPEED)
                                     (make-invader 40 80 INVADER-SPEED)))
              (list (make-invader 40 80 INVADER-SPEED)))

(check-expect (compare-missile (make-missile 100 150 )
                               (list (make-invader 40 80 INVADER-SPEED)
                                     (make-invader 100 150 INVADER-SPEED)))
              (list (make-invader 40 80 INVADER-SPEED)))

(check-expect (compare-missile (make-missile 100 150 )
                               (list (make-invader 40 80 INVADER-SPEED)
                                     (make-invader 150 200 INVADER-SPEED)))
              (list (make-invader 40 80 INVADER-SPEED)
                    (make-invader 150 200 INVADER-SPEED))) 

;(define (compare-missile m loi) loi) ;stub
(define (compare-missile m loi)
  (cond [(empty? loi) empty]
        [else
         (if (hit? (first loi) m)
             (rest loi)
             (cons (first loi) (compare-missile m (rest loi))))]))



;; ListOfInvader ListOfMissile -> ListOfMissile
;; produces neat and tidy list of missiles

(check-expect (compare-lom empty empty)
              empty)

(check-expect (compare-lom (list I1)
                           (list M1))
              (list M1))

(check-expect (compare-lom (list (make-invader 100 150 INVADER-SPEED))
                           (list (make-missile 100 155)))
              empty)

(check-expect (compare-lom (list (make-invader 100 150 INVADER-SPEED)
                                 (make-invader 120 170 INVADER-SPEED))
                           (list (make-missile 100 155)
                                 (make-missile 120 175)))
              empty)

(check-expect (compare-lom (list (make-invader 100 150 INVADER-SPEED)
                                 (make-invader 120 170 INVADER-SPEED))
                           (list (make-missile 100 155)
                                 (make-missile 40 80)))
              (list (make-missile 40 80)))

(check-expect (compare-lom (list (make-invader 100 150 INVADER-SPEED)
                                 (make-invader 120 170 INVADER-SPEED))
                           (list (make-missile 40 80)
                                 (make-missile 100 155)))
              (list (make-missile 40 80)))

;(define (compare-lom loi lom) lom) ;stub
;<template of compare-lom>

(define (compare-lom loi lom)
  (cond [(empty? loi) lom]
        [else
         (compare-lom (rest loi) (compare-invader (first loi) lom))]))


;; Invader ListOfMissile -> ListOfMissile
;; remove the hit missile from the ListOfMissile

(check-expect (compare-invader I1 empty)
              empty)

(check-expect (compare-invader (make-invader 100 150 INVADER-SPEED)
                               (list (make-missile 100 155)
                                     (make-missile 40 80)))
              (list (make-missile 40 80)))

(check-expect (compare-invader (make-invader 100 150 INVADER-SPEED)
                               (list (make-missile 40 80)
                                     (make-missile 100 155)))
              (list (make-missile 40 80)))

(check-expect (compare-invader (make-invader 100 150 INVADER-SPEED)
                               (list (make-missile 40 80)
                                     (make-missile 150 200)))
              (list (make-missile 40 80)
                    (make-missile 150 200))) 
  
;(define (compare-invader i lom) lom) ;stub

(define (compare-invader i lom)
  (cond [(empty? lom) empty]
        [else
         (if (hit? i (first lom))
             (rest lom)
             (cons (first lom) (compare-invader i (rest lom))))]))


;; Invader Missile -> Boolean
;; compares the x and y coordinates of the invader and missile and produces true if they are in the HIT RANGE

(check-expect (hit? (make-invader 100 150 INVADER-SPEED)
                    (make-missile 30 60))
              false)

(check-expect (hit? (make-invader 100 150 INVADER-SPEED)
                    (make-missile 100 155))
              true)

;(define (hit? i m) false) ;stub
(define (hit? i m)
  (and (<= (abs (- (missile-x m) (invader-x i))) (/ (image-width INVADER) 2)) 
       (>= 10 (- (missile-y m) (invader-y i)) 0)))

 

;; game -> Image
;; renders image of the game containing invaders, missiles and a tank

;base case ( ONLY TANK )
(check-expect (render-game
               (make-game empty empty (make-tank 150 1)))
              (place-image TANK 150 (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))

;( TANK & INVADER )
(check-expect (render-game
               (make-game
                (list (make-invader 150 100 INVADER-SPEED))
                empty
                (make-tank 150 1)))
              (place-image INVADER 150 100
                           (place-image TANK 150 (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND)))

; ( TANK & MISSILE )
(check-expect (render-game
               (make-game
                empty
                (list (make-missile 120 140))
                (make-tank 150 1)))
              (place-image MISSILE 120 140
                           (place-image TANK 150 (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND)))

; ( TANK & INVADER * MISSILE )
(check-expect (render-game
               (make-game
                (list (make-invader 150 100 INVADER-SPEED))
                (list (make-missile 120 140))
                (make-tank 150 1)))
              (place-image INVADER 150 100
                           (place-image MISSILE 120 140                                        
                                        (place-image TANK 150 (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))))

              

;(define (render-game g) BACKGROUND) ;stub
;<use template from game>

(define (render-game g)
  (render-loi (game-invaders g) (render-lom (game-missiles g) (render-tank (game-tank g)))))
       
;; tank -> Image
;; renders the given TANK in the appropriate place on the BACKGROUND

(check-expect (render-tank (make-tank 150 1))
              (place-image TANK 150 (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))

;(define (render-tank g) BACKGROUND) ;stub
;<use template from tank>
(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT (/ (image-height TANK) 2)) BACKGROUND))

;; ListOfMissile Image -> Image
;; renders the given ListOfMissile on the given Image

;base case (empty lom)
(check-expect (render-lom
               empty
               (render-tank (make-tank 150 1)))
              (render-tank (make-tank 150 1)))

;(one long ListOfMissile)
(check-expect (render-lom
               (list (make-missile 100 150))
               (render-tank (make-tank 150 1)))
              (place-image MISSILE 100 150
                           (render-tank (make-tank 150 1))))

;(two long ListOfMissile)
(check-expect (render-lom
               (list
                (make-missile 100 150)
                (make-missile 110 160))
               (render-tank (make-tank 150 1)))
              (place-image MISSILE 100 150
                           (place-image MISSILE 110 160                                        
                                        (render-tank (make-tank 150 1)))))
              

;(define (render-lom lom i) BACKGROUND) ;stub
;<use template from lom>
(define (render-lom lom i)
  (cond [(empty? lom) i]
        [else
         (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))               
                      (render-lom (rest lom) i))]))


;; loi Image -> Image
;; renders the given ListOfInvader on the given Image

;base case ( empty loi )
(check-expect (render-loi empty
                          (render-lom
                           (list
                            (make-missile 100 150)
                            (make-missile 110 160))
                           (render-tank (make-tank 150 1))))
              (render-lom
               (list
                (make-missile 100 150)
                (make-missile 110 160))
               (render-tank (make-tank 150 1))))

;one long list (ListOfInvader)
(check-expect (render-loi (list
                           (make-invader 70 80 INVADER-SPEED))
                          (render-lom
                           (list
                            (make-missile 100 150)
                            (make-missile 110 160))
                           (render-tank (make-tank 150 1))))
              (place-image INVADER 70 80
                           (render-lom
                            (list
                             (make-missile 100 150)
                             (make-missile 110 160))
                            (render-tank (make-tank 150 1)))))

;two long list (ListOfInvader)
(check-expect (render-loi (list
                           (make-invader 70 80 INVADER-SPEED)
                           (make-invader 40 50 INVADER-SPEED))
                          (render-lom
                           (list
                            (make-missile 100 150)
                            (make-missile 110 160))
                           (render-tank (make-tank 150 1))))
              (place-image INVADER 70 80
                           (place-image INVADER 40 50
                                        (render-lom
                                         (list
                                          (make-missile 100 150)
                                          (make-missile 110 160))
                                         (render-tank (make-tank 150 1))))))
              

;(define (render-loi loi i) BACKGROUND) ;stub
;<use template from loi>
(define (render-loi loi i)
  (cond [(empty? loi) i]
        [else         
         (place-image INVADER (invader-x (first loi)) (invader-y (first loi))              
                      (render-loi (rest loi) i))]))     

;; game KeyEvent -> game
;; - triggers a missile whenever a spacebar is pressed
;; - moves the tank to the right continuously whenever the right arrow is pressed 
;; - moves the tank to the left continuously whenever the left arrow is pressed

; spacebar case
(check-expect (handle-key (make-game empty empty (make-tank 150 1)) " ")
              (make-game empty
                         (list
                          (make-missile 150 (- HEIGHT TANK-HEIGHT/2)))
                         (make-tank 150 1)))

; right-arrow case tank going right
(check-expect (handle-key (make-game empty empty (make-tank 150 1)) "right")
              (make-game empty empty (make-tank (+ 150 TANK-SPEED) 1)))

; right-arrow case tank going left
(check-expect (handle-key (make-game empty empty (make-tank 150 -1)) "right")
              (make-game empty empty (make-tank (+ 150 TANK-SPEED) 1)))

; left-arrow case tank going right
(check-expect (handle-key (make-game empty empty (make-tank 150 1)) "left")
              (make-game empty empty (make-tank (- 150 TANK-SPEED) -1)))

; left-arrow case tank going left
(check-expect (handle-key (make-game empty empty (make-tank 150 -1)) "left")
              (make-game empty empty (make-tank (- 150 TANK-SPEED) -1)))

;<use template from Website handle-key func.>
(define (handle-key g k)
  (cond [(key=? k " ")
         (make-game (game-invaders g)
                    (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles g))
                    (game-tank g))]
        [(key=? k "right")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (+ (tank-x (game-tank g)) TANK-SPEED) 1))]
        [(key=? k "left")
         (make-game (game-invaders g)
                    (game-missiles g)
                    (make-tank (- (tank-x (game-tank g)) TANK-SPEED) -1))]
        [else
         g]))
        
;(define (handle-key g k) G0) ;stub

;; game -> Boolean
;; ends the game whenever an invader exactly lands

;base case ( no invaders )
(check-expect (game-over (make-game empty
                                    empty
                                    T0))
              false)

;1st case ( no invader landed )
(check-expect (game-over (make-game
                          (list (make-invader 100 150 INVADER-SPEED)
                                (make-invader 200 400 INVADER-SPEED))
                          empty
                          T0))
              false)

;2nd case ( invader exactly landed )
(check-expect (game-over (make-game
                          (list (make-invader 100 150 INVADER-SPEED)
                                (make-invader 200 (- HEIGHT  (/ (image-height INVADER) 2)) INVADER-SPEED))
                          empty
                          T0))
              true)



;(define (game-over g) false) ;stub
(define (game-over g)
  (cond [(empty? (game-invaders g)) false]
        [else
         (if (>= (invader-y (first (game-invaders g))) (- HEIGHT  (/ (image-height INVADER) 2)))
             true
             (game-over (make-game (rest (game-invaders g))
                                   (game-missiles g)
                                   (game-tank g))))])) 
  


;; Game -> Image
;; produces GAME-OVER image when the game is over

(define (last-picture g)
  GAME-OVER)














