#lang scheme
(require "../syn.ss")

(reactimate!
 (define-event (seconds [last #f])
   (define a1 (+ 1 seconds))
   (define a2 (+ 2 seconds))
   (if (even? seconds)
       a1
       (+ a1 a2))))