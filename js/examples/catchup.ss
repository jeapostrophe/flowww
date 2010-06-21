#lang scheme
(require "../js.ss"
         web-server/servlet-env)

(define (start req)
  `(html
    (head 
     (title "Catch Up")
     (style ([type "text/css"])
            #<<END
        body { margin: 0px; overflow: hidden;}
	
	#frameRateContainer {
		position: absolute; 
		bottom: 0px; 
		padding: 5px 0px 5px 0px;  
		background-color: #000; 
		color: #FFF; 
		display: block; 
		width: 100%;
		letter-spacing: .1em;

	}
	
	#frameRate, #delayRate {
		border: 0px #000000 solid; 
		padding: 5px 0px 5px 0px; 
		font-weight: bold; 
		text-align: center;
	}
END
            )
     (script ([type "text/javascript"] [src "/flapjax.js"]) 
             "")
     ,(javascript
       (function loader ()
                 (flapjaxInit)
                 (var (DIAM1 100))
                 (var (DIAM2 220))
                 (var (OFFSET2 23))
                 (var (SPEED 1000))
                 (var (toInt (function (v def) (try (block (return (parseInt v))) (catch _ (block (return def)))))))
                 (var (timerB (timer_b ((field (extractValue_b "frameRate") lift_b) toInt 50))))
                 (var (delayTimeB ((field (extractValue_b "delayRate") transform_b) toInt 500)))
                 (insertDomB
                  (DIVB
                   (object
                    (id "catch")
                    (style
                     (object
                      (backgroundColor "#000000")
                      (color "#FFFFFF")
                      (fontSize "3em")
                      (position "absolute")
                      (padding "5px")
                      (left
                       (apply_b
                        (function (t x) (return (+ x ((field Math round) (* DIAM1 ((field Math cos) (/ t SPEED)))))))
                        timerB
                        (mouseLeft_b document)))
                      (top
                       (apply_b
                        (function (t y) (return (+ y ((field Math round) (* DIAM1 ((field Math sin) (/ t SPEED)))))))
                        timerB
                        (mouseTop_b document))))))
                   "catch")
                  "body"
                  "beginning")
                 (var
                  (upB
                   (tagRec
                    (array "mouseover" "mouseout")
                    (function
                     (mouseOverE mouseOutE)
                     (return
                      (DIVB
                       (object
                        (id "up")
                        (style
                         (object
                          (padding "5px")
                          (backgroundColor
                           ((field (merge_e ((field mouseOverE constant_e) "#FF0000") 
                                            ((field mouseOutE constant_e) "#000"))
                                   startsWith)
                            "#000"))
                          (color "#FFFFFF")
                          (fontSize "3em")
                          (position "absolute")
                          (left
                           (apply_b
                            (function (t x) (return (+ (+ OFFSET2 x) ((field Math round) (* DIAM2 ((field Math cos) (/ t SPEED)))))))
                            timerB
                            ((field (mouseLeft_b document) delay_b) delayTimeB)))
                          (top
                           (apply_b
                            (function (t y) (return (+ y ((field Math round) (* DIAM2 ((field Math sin) (/ t SPEED)))))))
                            timerB
                            ((field (mouseTop_b document) delay_b) delayTimeB))))))
                       "up"))))))
                 (insertDomB upB "catch" "before")
                 (var
                  (caughtUpB
                   ((field ((field (extractEvent_e upB "mouseover") collect_e) 0 (function (_ p) (return (+ p 1)))) startsWith) 0)))
                 (insertDomB
                  (DIVB
                   (H1B
                    "you caught up "
                    (SPANB
                     (object (style (object (color "white") (backgroundColor "black"))))
                     ((field caughtUpB lift_b) (function (n) (return ((field n toString))))))
                    " times.")
                   "hit up with your mouse")
                  "count"))))
    (body ([onload ,(js-expr (loader))]
           [id "body"]
           [style "text-align: center;"])
          (div ([id "frameRateContainer"])
               "update every " (input ([type "text"] [id "frameRate"] [value "10"] [size "5"])) " ms "
               "with a delay of " (input ([type "text"] [id "delayRate"] [value "800"] [size "5"])) " ms")
          (span ([id "count"]) "count"))))

(serve/servlet 
 start
 #:extra-files-paths
 (list
  (build-path "/Users/jay/Dev/svn/byu-plt/trunk/flowww/")))