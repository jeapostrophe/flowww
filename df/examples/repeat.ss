#lang scheme
(require "../df.ss")

(define added
  (lift (lambda (s) 
          (printf "Adding~n")
          (add1 s))
        seconds-evt))

(reactimate! 
 (lift = added added))