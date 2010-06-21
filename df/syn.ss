#lang scheme
(require (only-in "df.ss"
                  dt reactimate-thunk!
                  values->list))

(define-syntax (seconds etx) (error 'seconds "Identifier used out of event"))

(define-syntax (define-event stx)
  (syntax-case stx (define)
    [(_ (current-seconds lv ...)
        (define x x-expr)
        ...
        expr)
     (syntax/loc stx
       (lambda (current-seconds lv ...)
         (define x x-expr)
         ...
         expr))]))

(define (reactimate! v)
  (define vs empty)
  (reactimate-thunk!
   (lambda (nt)
     (define nvs (values->list (apply v nt vs)))
     (set! vs nvs)
     nvs)))

(provide seconds define-event
         reactimate!)