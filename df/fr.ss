#lang scheme/base

(define-struct an-undefined ())
(define undefined (make-an-undefined))
(define (undefined? v) (eq? v undefined))

(define-struct behavior ())
(define-struct event ())

(define (signal? v) (or (behavior? v) (event? v)))

(define seconds (make-behavior))
(define milliseconds (make-behavior))
(define never-e (make-event))

; XXX
(define (new-cell b0)
  (make-behavior))
(define (set-cell! c v)
  (void))

; XXX
(define (new-event)
  (make-event))
(define (send-event! e v)
  (void))

; XXX
(define (value-now b)
  undefined)

; XXX
(define (delay-by b n)
  (make-behavior))

; XXX
(define (integral b)
  (make-behavior))

; XXX
(define (derivative b)
  (make-behavior))

; XXX
(define (map-e f e)
  (make-event))
(define (==> e f)
  (map-e f e))

; XXX
(define (filter-e p e)
  (make-event))
(define (=#> e p)
  (filter-e p e))

; XXX
(define (merge-e . es)
  (make-event))

; XXX
(define (once-e e)
  (make-event))

; XXX
(define (changes b)
  (make-event))

; XXX
(define (hold e v)
  (make-behavior))

; XXX
(define (switch e b)
  ; e may carry a behavior
  (make-behavior))

; XXX
(define (accum-e ef v)
  (make-event))

(define (accum-b e v)
  (hold (accum-e e v) v))

; XXX
(define (collect-e e v p)
  (make-event))

(define (collect-b e v p)
  (hold (collect-e e v p) v))

; XXX
(define (when-e b)
  (make-event))

; XXX
(define (lift-strict p . vs)
  (make-behavior))
