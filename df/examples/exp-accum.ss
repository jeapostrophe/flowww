#lang scheme
(require "../syn.ss")

(reactimate!
 (define-event (seconds [last 0])
   (add1 last)))