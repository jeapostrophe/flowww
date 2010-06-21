#lang scheme

(define-struct EVT ())

(define-struct (switch EVT) (cond true false))
(define-struct (_evt EVT) (deps f [last-t #:mutable] [last-v #:mutable]))
(define-struct (_seconds-evt EVT) ())

(define seconds-evt (make-_seconds-evt))

(define (make-evt deps f)
  (make-_evt deps f #f #f))

(define (lift f . args)
  (make-evt args f))

(define (collect e iv update)
  (define this-delayed (make-delay (list iv) 0 #f))
  (define this (lift update e this-delayed))
  (set-delay-evt! this-delayed this)
  this)

(define-struct delay (lv lt evt) #:mutable)
 
(define-syntax-rule (values->list e)
  (call-with-values (lambda () e) (lambda x x)))
(define (list->values l) (apply values l))

(define (evaluate nt e)
  (match e
    [(? _seconds-evt? s)
     nt]
    [(struct switch (cond true false))
     (if (evaluate nt cond)
         (evaluate nt true)
         (evaluate nt false))]
    [(struct delay (lv lt evt))
     (if (= lt nt)
         (list->values lv)
         (local [(define new-nt (- nt dt))
                 (define new-lv (values->list (evaluate new-nt evt)))]
           (set-delay-lt! e new-nt)
           (set-delay-lv! e new-lv)
           (list->values new-lv)))]     
    [(struct _evt (deps f last-t last-v))
     (if (and last-t (= last-t nt))
         (list->values last-v)
         (local [(define nvs (values->list (apply f (map (curry evaluate nt) deps))))]
           (set-_evt-last-t! e nt)
           (set-_evt-last-v! e nvs)
           (list->values nvs)))]
    [x x]))

(define dt 1)
(define (reactimate! evt)
  (reactimate-thunk! (lambda (nt) (evaluate nt evt))))

(define (reactimate-thunk! thnk)
  (define c 0)
  (let loop ([nt 0])
    (set! c (add1 c))
    (when (c . > . 10) (exit 0))
    
    (printf "~S~n" (thnk nt))
    (loop (+ nt dt)))
  (void))

(provide (all-defined-out))