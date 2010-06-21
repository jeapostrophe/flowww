(module as-flight-lander "as/syn.ss"
#|  
  ;; DATA:
  ;; The World is a
  ;;
  ;; (make-world plane balloons fuel)
  ;; where:
  ;;   plane is a posn
  ;;   balloon is a list of balloon-info
  ;;   fuel is a num
  (define-struct world (plane balloons fuel))
  
  ;; A balloon-info is a 
  ;; (make-balloon-info position direction)
  ;; where:
  ;;   position is a posn
  ;;   direction is a num
  (define-struct balloon-info (position direction))
  
  (define (rectangle a b c d)
    5)
  (define (image-width e)
    10)
  (define (place-image a b c d)
    15)
  (define nw:rectangle rectangle)
  (define (empty-scene a b)
    20)
  
  (define PLANE (rectangle 35 15 'solid 'red))
  
  ;; WIDTH of BACKGROUND
  (define WIDTH 800)
  
  ;; HEIGHT of BACKGROUND
  (define HEIGHT 500)
  
  ;; PLANE-MOVE-X: the amount that the plane should move horizontally per tick.
  (define PLANE-MOVE-X 20)
  
  ;; PLANE-MOVE-Y: the amount that the plane descends per tick.
  (define PLANE-MOVE-Y 5)
  
  ;; KEY-DISTANCE: the amount of vertical ascent or descent on key presses.
  (define KEY-DISTANCE 10)
  
  ;; BASE-HEIGHT of the ground.
  (define BASE-HEIGHT 50)
  
  ;; WIDTH of the water.
  (define WATER-WIDTH 500)
  
  ;; IMAGE: image
  (define WATER (nw:rectangle WATER-WIDTH BASE-HEIGHT "solid" "blue"))
  
  ;; IMAGE: image
  (define LAND (nw:rectangle (- WIDTH WATER-WIDTH) BASE-HEIGHT "solid" "brown"))
  
  
  (define BALLOON-IMG (rectangle 25 25 'solid 'blue)) 
  
  
  
  ;; THRESHOLD is the radius of pixels of the hitbox for collisions.
  (define THRESHOLD (* (image-width BALLOON-IMG) 2/3))
  
  
  ;; BACKGROUND: scene
  (define BACKGROUND
    (place-image WATER
                 0
                 (- HEIGHT BASE-HEIGHT)
                 (place-image LAND
                              WATER-WIDTH
                              (- HEIGHT BASE-HEIGHT)
                              (empty-scene WIDTH HEIGHT))))
  
  ;; FUNCTIONS:
  
  ;; move-plane-wrapping-x-on-tick: number -> number
  ;; increase the x-position of PLANE by PLANE-MOVE-X modulo WIDTH
  (define (move-plane-wrapping-x-on-tick x)
    (modulo (+ x PLANE-MOVE-X)
            WIDTH))
  
  ;; TEST:
  (check-expect (move-plane-wrapping-x-on-tick 10) (modulo (+ 10 PLANE-MOVE-X) WIDTH))
  (check-expect (move-plane-wrapping-x-on-tick 0) (modulo (+ 0 PLANE-MOVE-X) WIDTH))
  (check-expect (move-plane-wrapping-x-on-tick 800) (modulo (+ 800 PLANE-MOVE-X) WIDTH))
  
  
  ;; move-plane-xy-on-tick: posn -> posn
  ;; increase the y-position of PLANE by PLANE-MOVE-y
  (define (move-plane-xy-on-tick p)
    (make-posn (move-plane-wrapping-x-on-tick (posn-x p))
               (+ (posn-y p) PLANE-MOVE-Y)))
  
  ;; TEST:
  (check-expect 
   (move-plane-xy-on-tick (make-posn 10 20)) 
   (make-posn (modulo (+ 10 PLANE-MOVE-X) WIDTH) (+ 20 PLANE-MOVE-Y)))
  
  (check-expect 
   (move-plane-xy-on-tick (make-posn 0 30)) 
   (make-posn (modulo (+ 0 PLANE-MOVE-X) WIDTH) (+ 30 PLANE-MOVE-Y)))
  
  (check-expect 
   (move-plane-xy-on-tick (make-posn 800 40)) 
   (make-posn (modulo (+ 800 PLANE-MOVE-X) WIDTH) (+ 40 PLANE-MOVE-Y)))
  
  
  ;; move-balloon-on-tick: balloon-info -> balloon-info
  ;; moves the balloon based on its direction.
  (define (move-balloon-on-tick b)
    (make-balloon-info (make-posn (posn-x (balloon-info-position b))
                                  (+ (posn-y (balloon-info-position b))
                                     (balloon-info-direction b)))
                       (balloon-info-direction b)))
  
  ;; TEST
  (check-expect (move-balloon-on-tick (make-balloon-info (make-posn 50 50) 1))
                (make-balloon-info (make-posn 50 51) 1))
  (check-expect (move-balloon-on-tick (make-balloon-info (make-posn 50 50) -4))
                (make-balloon-info (make-posn 50 46) -4))
  
  
  ;; move-balloons-on-tick: (listof balloon) -> (listof balloon)
  ;; Moves all of the balloons.
  (define (move-balloons-on-tick balloons)
    (cond
      [(empty? balloons)
       empty]
      [else
       (cons (move-balloon-on-tick (first balloons))
             (move-balloons-on-tick (rest balloons)))]))
  
  ;; TEST
  (check-expect (move-balloons-on-tick empty) empty)
  
  (check-expect (move-balloons-on-tick
                 (cons (make-balloon-info (make-posn 50 50) 1)
                       (cons (make-balloon-info (make-posn 50 50) -4)
                             empty)))
                (cons (make-balloon-info (make-posn 50 51) 1)
                      (cons (make-balloon-info (make-posn 50 46) -4)
                            empty)))
  
  
  ;; move-plane-and-balloons-on-tick: world -> world
  ;; Moves the plane and balloons in the world.
  (define (move-plane-and-balloons-on-tick w)
    (make-world (move-plane-xy-on-tick (world-plane w))
                (move-balloons-on-tick (world-balloons w))
                (world-fuel w)))
  
  
  ;; TEST:
  (check-expect (move-plane-and-balloons-on-tick 
                 (make-world (make-posn 0 0)
                             (cons (make-balloon-info (make-posn 30 40) -1) empty)
                             0))
                (make-world (make-posn PLANE-MOVE-X PLANE-MOVE-Y)
                            (cons (make-balloon-info (make-posn 30 39) -1) empty)
                            0))
  
  (check-expect (move-plane-and-balloons-on-tick
                 (make-world (make-posn WIDTH 0) 
                             (cons (make-balloon-info (make-posn 50 50) 1) empty)
                             0))
                (make-world (make-posn PLANE-MOVE-X PLANE-MOVE-Y)
                            (cons (make-balloon-info (make-posn 50 51) 1) empty)
                            0))
  
  
  
  ;; alter-plane-y-fuel-on-key: world a-key -> world
  ;; lift the y-position by KEY-DISTANCE when user presses 'up
  ;; drop the y-position by KEY-DISTANCE when user presses 'down
  (define (alter-plane-y-fuel-on-key w a-key)
    (cond
      [(key=? a-key 'up)
       (cond 
         [(> (world-fuel w) 0)
          (make-world
           (make-posn (posn-x (world-plane w))
                      (max 0 (- (posn-y (world-plane w)) KEY-DISTANCE)))
           (world-balloons w)
           (sub1 (world-fuel w)))]
         [else
          ;; if there's no fuel left, ignore the keystroke
          w])] 
      [(key=? a-key 'down)
       ;; going down doesn't consume fuel
       (make-world (make-posn (posn-x (world-plane w))
                              (min HEIGHT (+ (posn-y (world-plane w)) KEY-DISTANCE)))
                   (world-balloons w)
                   (world-fuel w))]
      [else w]))
  
  ;; TEST
  ;; SAMPLE-BALLOON-INFOS is used in the test cases below.
  (define SAMPLE-BALLOON-INFOS (cons (make-balloon-info (make-posn 300 200) -3) 
                                     empty))
  (check-expect (alter-plane-y-fuel-on-key (make-world (make-posn 10 20) 
                                                       SAMPLE-BALLOON-INFOS
                                                       100)
                                           'up) 
                (make-world (make-posn 10  (- 20 KEY-DISTANCE)) 
                            SAMPLE-BALLOON-INFOS
                            99))
  ;; test on no gas
  (check-expect (alter-plane-y-fuel-on-key 
                 (make-world (make-posn 10 20)
                             SAMPLE-BALLOON-INFOS
                             0) 
                 'up) 
                (make-world (make-posn 10 20) 
                            SAMPLE-BALLOON-INFOS
                            0))
  
  (check-expect (alter-plane-y-fuel-on-key (make-world (make-posn 0 30)
                                                       SAMPLE-BALLOON-INFOS
                                                       100) 
                                           'down) 
                (make-world (make-posn 0 (+ 30 KEY-DISTANCE)) 
                            SAMPLE-BALLOON-INFOS
                            100))
  (check-expect (alter-plane-y-fuel-on-key (make-world (make-posn 40 50) 
                                                       SAMPLE-BALLOON-INFOS
                                                       50) 'a) 
                (make-world (make-posn 40 50)
                            SAMPLE-BALLOON-INFOS
                            50))
  
  
  ;; on-land-or-water?: posn -> boolean
  ;; determine whether or not PLANE has touched one of LAND and WATER
  (define (on-land-or-water? p)
    (>= (posn-y p) (- HEIGHT BASE-HEIGHT)))
  
  ;; TEST
  (check-expect (on-land-or-water? (make-posn 40 0)) false)
  (check-expect (on-land-or-water? (make-posn 40 (- HEIGHT BASE-HEIGHT))) true)
  
  
  ;; distance: posn posn -> number
  ;; Computes the distance between the two posns.
  (define (distance p1 p2)
    (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
             (sqr (- (posn-y p1) (posn-y p2))))))
  
  (check-expect (distance (make-posn 0 0) (make-posn 0 0)) 0)
  (check-expect (distance (make-posn 1 0) (make-posn 1 1)) 1)
  (check-within (distance (make-posn 0 0) (make-posn 1 1)) (sqrt 2) 1/1000000)
  
  
  ;; overlapping?: posn posn -> boolean
  ;; Determines whether the plane-posn and balloon-posn are overlapping.
  (define (overlapping? plane-posn balloon-posn)
    (< (distance plane-posn balloon-posn)
       THRESHOLD))
  
  ;; TEST
  (check-expect (overlapping? (make-posn 0 0)
                              (make-posn 1000 0))
                false)
  (check-expect (overlapping? (make-posn 50 49)
                              (make-posn 51 51))
                true)
  
  
  ;; plane-overlapping-any-balloon?: posn (listof balloon-info) -> boolean
  ;; Determines whether the plane-posn is overlapping any of the balloons.
  (define (plane-overlapping-any-balloon? plane-posn balloons)
    (cond
      [(empty? balloons)
       false]
      [else
       (or (overlapping? plane-posn (balloon-info-position (first balloons)))
           (plane-overlapping-any-balloon? plane-posn (rest balloons)))]))
  
  ;; TEST
  (check-expect (plane-overlapping-any-balloon? (make-posn 0 0) empty)
                false)
  (check-expect (plane-overlapping-any-balloon? (make-posn 0 0) 
                                                (cons (make-balloon-info 
                                                       (make-posn 0 0) 0)
                                                      empty))
                true)
  (check-expect (plane-overlapping-any-balloon? (make-posn 0 0) 
                                                (cons (make-balloon-info 
                                                       (make-posn 1000 1000) 0)
                                                      empty))
                false)
  (check-expect (plane-overlapping-any-balloon? (make-posn 0 0) 
                                                (cons (make-balloon-info 
                                                       (make-posn 1000 1000) 0)
                                                      (cons (make-balloon-info
                                                             (make-posn 0 0) 0)
                                                            empty)))
                true)
  
  
  
  ;; game-ends?: world -> boolean
  ;; Determines whether the game is over.
  (define (game-ends? w)
    (cond
      [(on-land-or-water? (world-plane w)) 
       true]
      [(plane-overlapping-any-balloon? (world-plane w) 
                                       (world-balloons w))
       true]
      [else 
       false]))
  
  ;; TEST
  (check-expect (game-ends? 
                 (make-world (make-posn 0 (+ HEIGHT (image-height PLANE)))
                             SAMPLE-BALLOON-INFOS
                             42))
                true)
  (check-expect (game-ends? (make-world (balloon-info-position (first SAMPLE-BALLOON-INFOS))
                                        SAMPLE-BALLOON-INFOS
                                        42))
                true)
  (check-expect (game-ends? (make-world (make-posn 0 0) 
                                        SAMPLE-BALLOON-INFOS
                                        42))
                false)
  
  
  ;; place-plane-xy: posn -> Scene
  ;; place PLANE onto BACKGROUND
  (define (place-plane-xy p)
    (place-image WATER
                 0
                 (- HEIGHT BASE-HEIGHT)
                 (place-image PLANE 
                              (posn-x p)
                              (posn-y p)
                              BACKGROUND)))
  
  ;; place-balloon: balloon-info scene -> scene
  ;; Places the balloon into the scene.
  (define (place-balloon b scene)
    (place-image BALLOON-IMG
                 (posn-x (balloon-info-position b))
                 (posn-y (balloon-info-position b))
                 scene))
  
  ;; place-balloons: (listof balloon-info) scene -> scene
  ;; Places all balloons into the scene.
  (define (place-balloons balloons scene)
    (cond
      [(empty? balloons)
       scene]
      [else
       (place-balloons (rest balloons)
                       (place-balloon (first balloons) scene))]))
  
  
  ;; place-world: world -> Scene
  ;; Produces a scene with the plane.
  (define (place-world world)
    (place-balloons (world-balloons world)
                    (place-plane-xy (world-plane world))))
  
  
  
  
  ;; BALLOON-1 is a balloon drifting upward. 
  (define BALLOON-1 (make-balloon-info (make-posn 600 300) -3))
  ;; BALLOON-2 is a balloon drifting downward.
  (define BALLOON-2 (make-balloon-info (make-posn 30 370) 1))
  ;; BALLOON-3 is a balloon that's stationary
  (define BALLOON-3 (make-balloon-info (make-posn 400 230) 0))
  
  (define (play)
    (begin
      ;; RUN PROGRAM
      (big-bang WIDTH HEIGHT 1/30 (make-world (make-posn 0 0)
                                              (cons BALLOON-1
                                                    (cons BALLOON-2
                                                          (cons BALLOON-3 empty)))
                                              20))
      (on-tick-event move-plane-and-balloons-on-tick)
      (on-key-event alter-plane-y-fuel-on-key)
      (on-redraw place-world)
      (stop-when game-ends?)
      ))
|#
  
  (cond
      [1 2]
      [else 3])
  
  )