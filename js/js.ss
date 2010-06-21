#lang scheme
(require xml
         (planet dherman/javascript:9:0)
         (planet dherman/pprint:4))

(define-syntax javascript:
  (syntax-rules ()
    [(_ js)
     (Expression->javascript: (syntax->expression #`js))]))

(define (Expression->javascript: e)
  (string-append "javascript:" (pretty-format (format-expression e))))

(define-syntax js-expr
  (syntax-rules ()
    [(_ js)
     (Expression->javascript (syntax->expression #`js))]))

(define (Expression->javascript e)
  (pretty-format (format-expression e)))

(define-syntax javascript
  (syntax-rules ()
    [(_ js ...)
     `(script ([type "text/javascript"])
              ,(SourceElement->xexpr (syntax->source-element #`js))
              ...)]))

(define (SourceElement->xexpr se)
  (make-cdata #f #f (pretty-format (format-source-element se))))

(define (test)
  (display
   (xexpr->string
    (javascript: (alert "Hey!"))))
  
  (newline)
  
  (display 
   (xexpr->string
    (javascript (function foo (bar zog)
                          (return (bar (field-ref zog elem))))
                (foo (function (x) (return x)) (object [elem "y"]))))))

(provide js-expr
         javascript:
         javascript)