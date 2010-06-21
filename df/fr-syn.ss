#lang scheme
(require syntax/parse
         (for-template (prefix-in frp: frtime/lang-core)))

(define (compile-module-level-form stx)
  (syntax-parse 
   stx #:literal-sets (kernel-literals)
   [(#%provide expr)
    stx]
   [general-top-level-form
    (compile-general-top-level-form stx)]))

(define (compile-general-top-level-form stx)
  (syntax-parse 
   stx #:literal-sets (kernel-literals)
   [(define-values (id ...) expr)
    #`(define-values (id ...) #,(compile-expr expr))]
   [(define-syntaxes (id ...) expr)
    stx]
   [(define-values-for-syntax (id ...) expr)
    stx]
   [(#%require raw-require-spec ...)
    stx]
   [expr
    (compile-expr stx)]))

(define (compile-expr stx)
  (syntax-parse 
   stx #:literal-sets (kernel-literals)
   [id:id
    #'id]
   [(#%plain-lambda formals expr ...)
    (with-syntax ([(cexpr ...)
                   (map compile-expr (syntax->list #'(expr ...)))])
      #'(frp:lambda formals cexpr ...))]
   [(case-lambda case-sub ...)
    (with-syntax ([(ccase-sub ...)
                   (map (lambda (stx)
                          (syntax-case stx ()
                            [(formals expr ...)
                             (with-syntax ([(cexpr ...)
                                            (map compile-expr (syntax->list #'(expr ...)))])
                               #'(formals cexpr ...))]))
                        (syntax->list #'(case-sub ...)))])
      #'(frp:case-lambda ccase-sub ...))]
   [(if test true false)
    #`(frp:if #,(compile-expr #'test) 
              #,(compile-expr #'true)
              #,(compile-expr #'false))]
   ; XXX
   [e stx]))