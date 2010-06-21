#lang scheme
(require (prefix-in scheme: scheme))
(provide (rename-out [module-begin #%module-begin])
         define
         define-struct
         begin
         begin0
         set!
         lambda
         local
         letrec
         let
         let*
         recur
         #%app
         cond
         case
         if
         when
         unless
         and
         or
         empty
         quasiquote
         quote
         true
         false
         #%datum
         #%top)
(provide check-expect
         *
         +
         -
         /
         <
         <=
         =
         >
         >=
         abs
         acos
         add1
         angle
         asin
         atan
         ceiling
         complex?
         conjugate
         cos
         cosh
         current-seconds
         denominator
         ; e XXX
         even?
         exact->inexact
         exact?
         exp
         expt
         floor
         gcd
         imag-part
         inexact->exact
         inexact?
         integer->char
         integer?
         lcm
         log
         magnitude
         make-polar
         max
         min
         modulo
         negative?
         number->string
         number?
         numerator
         odd?
         pi
         positive?
         quotient
         random
         rational?
         real-part
         real?
         remainder
         round
         sgn
         sin
         sinh
         sqr
         sqrt
         sub1
         tan
         zero?
         boolean=?
         boolean?
         false?
         not
         symbol->string
         symbol=?
         symbol?
         append
         assq
         caaar
         caadr
         caar
         cadar
         cadddr
         caddr
         cadr
         car
         cdaar
         cdadr
         cdar
         cddar
         cdddr
         cddr
         cdr
         cons
         cons?
         eighth
         empty?
         fifth
         first
         fourth
         length
         list
         list*
         list-ref
         member
         memq
         memv
         null
         null?
         pair?
         rest
         reverse
         second
         seventh
         sixth
         third
         make-posn
         posn-x
         posn-y
         posn?
         char->integer
         char-alphabetic?
         char-ci<=?
         char-ci<?
         char-ci=?
         char-ci>=?
         char-ci>?
         char-downcase
         char-lower-case?
         char-numeric?
         char-upcase
         char-upper-case?
         char-whitespace?
         char<=?
         char<?
         char=?
         char>=?
         char>?
         char?
         format
         list->string
         make-string
         string
         string->list
         string->number
         string->symbol
         string-append
         string-ci<=?
         string-ci<?
         string-ci=?
         string-ci>=?
         string-ci>?
         string-copy
         string-length
         string-ref
         string<=?
         string<?
         string=?
         string>=?
         string>?
         string?
         substring
         ;image=? XXX
         ;image? XXX
         ;=~ XXX
         eof
         eof-object?
         eq?
         equal?
         ;equal~? XXX
         eqv?
         error
         exit
         identity
         struct?
         andmap
         apply
         build-list
         build-string
         compose
         filter
         foldl
         foldr
         for-each
         map
         memf
         ormap
         procedure?
         ; quicksort XXX
         sort)

; Resupply
(define-syntax module-begin
  (syntax-rules ()
    [(_ def-or-expr ...)
     (#%module-begin def-or-expr ...)]))

(define-syntax define
  (syntax-rules ()
    [(_ (func-id arg-id ...) expr)
     (scheme:define (func-id arg-id ...) expr)]
    [(_ val-id expr)
     (scheme:define val-id expr)]))

(define-syntax define-struct
  (syntax-rules ()
    [(_ struct-id (field-id ...))
     (scheme:define-struct struct-id (field-id ...))]))

(define-syntax lambda
  (syntax-rules ()
    [(_ (arg-id ...) expr)
     (scheme:lambda (arg-id ...) expr)]))

(define-syntax if
  (syntax-rules ()
    [(_ test true false)
     (scheme:if test true false)]))

; New
(define-struct posn (x y))
(define check-expect #f) ; XXX
(define identity (lambda (x) x))

(define-syntax begin
  (syntax-rules ()
    [(_ e)
     e]
    [(_ fe re ...)
     (let ([_ fe])
       (begin re ...))]))

(define-syntax begin0
  (syntax-rules ()
    [(_ e)
     e]
    [(_ fe re ...)
     (let ([tmp fe])
       (begin re ... tmp))]))

(define-syntax let
  (syntax-rules ()
    [(_ ([id expr] ...) body)
     ((lambda (id ...) body)
      expr ...)]
    [(_ loop ([id expr] ...) body)
     (recur loop ([id expr] ...) body)]))

(define-syntax let*
  (syntax-rules ()
    [(_ () body)
     body]
    [(_ ([fid fexpr] 
         [id expr] ...) body)
     (let ([fid fexpr])
       (let* ([id expr] ...)
         body))]))

(define-syntax recur
  (syntax-rules ()
    [(_ func-id ([arg-id arg-expr] ...) body-expr)
     ((local [(define (func-id arg-id ...)
                body-expr)]
        func-id)
      arg-expr ...)]))

(define-syntax letrec
  (syntax-rules ()
    [(_ ([id expr] ...) body)
     (local [(define id expr) ...]
       body)]))

(define-syntax cond
  (syntax-rules (else)
    [(_ [else result1])
     result1]
    [(_ [test result1])
     (when test result1)]
    [(_ [test result1]
           clause1 clause2 ...)
     (if test
         result1
         (cond clause1 clause2 ...))]))

(define-syntax case
  (syntax-rules (else)
    [(_ val-expr [(choice_1 choice_r ...)
                  expr]
        ...)
     (case val-expr [(choice_1 choice_r ...)
                     expr]
       ...
       [else (void)])]
    [(_ val-expr
        [(choice_1 choice_r ...)
         expr]
        ...
        [else else-expr])
     (let ([val val-expr])
       (cond
         [(memv? val '(choice_1 choice_r ...))
          expr]
         ...
         [else else-expr]))]))

(define-syntax when
  (syntax-rules ()
    [(_ e1 e2)
     (if e1 e2 (void))]))

(define-syntax unless
  (syntax-rules ()
    [(_ e1 e2)
     (if e1 (void) e2)]))

(define-syntax and
  (syntax-rules ()
    [(_)
     true]
    [(_ e1 er ...)
     (let ([tmp e1])
       (if tmp 
           (and er ...)
           tmp))]))

(define-syntax or
  (syntax-rules ()
    [(_)
     false]
    [(_ e1 er ...)
     (let ([tmp e1])
       (if tmp 
           tmp
           (or er ...)))]))

(define-syntax quote
  (syntax-rules ()
    [(_ (q ...))
     (list (quote q) ...)]
    [(_ q)
     (scheme:quote q)]))

(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    [(_ (unquote q))
     q]
    [(_ (unquote-splicing q))
     (error 'quasiquote "Cannot splicing in this position")]
    [(_ (qq ...))
     (append (quasiquote+ qq)
             ...)]
    [(_ q)
     (scheme:quote q)]))

(define-syntax quasiquote+
  (syntax-rules (unquote unquote-splicing)
    [(_ (unquote q))
     (list q)]
    [(_ (unquote-splicing q))
     q]
    [(_ (qq ...))
     (append (quasiquote+ qq)
             ...)]
    [(_ q)
     (list (scheme:quote q))]))
