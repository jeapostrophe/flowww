#lang scheme
(require "../df.ss")

(define add1-secs
  (make-evt (list seconds-evt)
            add1))

(define <-secs
  (make-evt (list seconds-evt add1-secs)
            <))

(reactimate! <-secs)