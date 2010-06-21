#lang scheme
(provide (all-defined-out))

(define (as-program->js-program p)
  (map as-def-or-expr->js-source-element p))

(define (as-def-or-expr->js-source-element as)
  ((as-definition->js-source-element 
    (as-test-case->js-source-element
     (as-library-require->js-source-element
      as-expr->js-source-element)))
   as))

(define ((as-test-case->js-source-element k) as)
  (k as))

(define ((as-library-require->js-source-element k) as)
  (k as))

(define ((as-definition->js-source-element k) d)
  (match d
    [`(define (,fun-name ,first-arg ,args ...) ,expr)
     `(function ,fun-name (,first-arg ,@args)
                ,(as-expr->js-source-element expr))]
    [`(define ,var-id ,expr)
     `(var [,var-id ,(as-expr->js-expr expr)])]
    [`(define-struct ,struct-id (,field-id ...))
     (define struct-label (gensym struct-id))
     (define make-struct-id (string->symbol (format "make-~a" struct-id)))
     (define struct-id? (string->symbol (format "~a?" struct-id)))
     (define struct-id-field-id (map (lambda (fi) (string->symbol (format "~a-~a" struct-id fi)))
                                     field-id))
     (define set-struct-id-field-id! (map (lambda (fi) (string->symbol (format "set-~a-~a!" struct-id fi)))
                                          field-id))
     (define field-id-num (build-list (length field-id) add1))
     `(block
       ,@(map (as-definition->js-source-element k)
              `((define (,make-struct-id ,@field-id)
                  (vector ',struct-label ,@field-id))
                (define (,struct-id? val)
                  (and (vector? val)
                       (equal? (vector-ref val 0) ',struct-label)))
                ,@(map
                   (lambda (struct-id-field-id field-id-num)
                     `(define (,struct-id-field-id the-struct)
                        (if (,struct-id? the-struct)
                            (vector-ref the-struct ,field-id-num)
                            (error ',struct-id-field-id ,(format "Must be passed ~a" struct-id)))))
                   struct-id-field-id
                   field-id-num)
                ,@(map
                   (lambda (set-struct-id-field-id! field-id-num)
                     `(define (,set-struct-id-field-id! the-struct the-val)
                        (if (,struct-id? the-struct)
                            (vector-set! the-struct ,field-id-num the-val)
                            (error ',set-struct-id-field-id! ,(format "Must be passed ~a" struct-id)))))
                   set-struct-id-field-id!
                   field-id-num))))]
    [_ (k d)]))

(define (as-expr->js-source-element as)
  (match as
    [`(begin ,fexpr ,rexpr ...)
     `(begin ,(as-expr->js-expr fexpr)
             ,@(map as-expr->js-expr rexpr))]
    [`(begin0 ,fexpr ,rexpr ...)
     (as-expr->js-expr
      (let ([tmp (gensym 'begin0)])
        `(local [(define ,tmp ,fexpr)]
           (begin ,@rexpr ,tmp))))]
    [`(set! ,id ,expr)
     `(= ,id ,(as-expr->js-expr expr))]
    [`(lambda (,id ...) ,expr)
     `(function (,@id) ,(as-expr->js-source-element expr))]
    [`(local [,definition ...] ,expr)
     `(block
       ,@(map as-definition->js-source-element definition)
       ,(as-expr->js-source-element expr))]
    [`(cond [else ,expr])
     (as-expr->js-source-element expr)]
    [`(cond [,texpr ,bexpr])
     (as-expr->js-source-element `(when ,texpr ,bexpr))]
    [`(cond [,texpr ,bexpr] ,rest ...)
     (as-expr->js-source-element `(if ,texpr ,bexpr (cond ,@rest)))]
    [`(case ,cexpr
        [(,fchoice ,rchoice ...) ,bexpr] ...
        [else ,eexpr])
     (as-expr->js-source-element 
      (let ([tmp (gensym 'case)])
        `(local [(define ,tmp ,cexpr)]
           (cond
             ,@(map (lambda (fchoice rchoice bexpr)
                      `[(memv ,tmp '(,fchoice ,@rchoice)) ,bexpr])
                    fchoice rchoice bexpr)
             [else ,eexpr]))))]
    [`(case ,cexpr
        [(,fchoice ,rchoice ...) ,bexpr] ...)
     (as-expr->js-source-element 
      (let ([tmp (gensym 'case)])
        `(local [(define ,tmp ,cexpr)]
           (cond
             ,@(map (lambda (fchoice rchoice bexpr)
                      `[(memv ,tmp '(,fchoice ,@rchoice)) ,bexpr])
                    fchoice rchoice bexpr)))))]
    [`(if ,cexpr ,texpr ,fexpr)
     `(if ,(as-expr->js-expr cexpr)
          ,(as-expr->js-substatement texpr)
          ,(as-expr->js-substatement fexpr))]
    [`(when ,cexpr ,bexpr)
     (as-expr->js-source-element `(if ,cexpr ,bexpr (void)))]
    [`(unless ,cexpr ,bexpr)
     (as-expr->js-source-element `(if ,cexpr (void) ,bexpr))]
    [`(and ,fexpr ,sexpr)
     `(&& ,(as-expr->js-expr fexpr)
          ,(as-expr->js-expr sexpr))]
    [`(and ,fexpr ,sexpr ,rexpr ...)
     (as-expr->js-source-element `(and (and ,fexpr ,sexpr) ,@rexpr))]
    [`(or ,fexpr ,sexpr)
     `(|| ,(as-expr->js-expr fexpr)
          ,(as-expr->js-expr sexpr))]
    [`(or ,fexpr ,sexpr ,rexpr ...)
     (as-expr->js-source-element `(or (or ,fexpr ,sexpr) ,@rexpr))]
    [`empty
     (as-expr->js-source-element `(list))]
    #;[prim-op
       ; XXX
       #f]
    [`(quote ,quoted)
     (as-expr->js-source-element (quote->expr quoted))]
    [`(quasiquote ,quasiquoted)
     (as-expr->js-source-element (quasiquote->expr quasiquoted))]
    [`(,fexpr ,aexpr ...)
     `(,(as-expr->js-expr fexpr)
       ,@(map as-expr->js-expr aexpr))]
    [`true
     #t]
    [`false
     #f]
    [(? symbol? id)
     ; XXX
     id]
    [(? number? n)
     n]
    [(? string? s)
     s]
    [(? char? c)
     c]))

(define (as-expr->js-expr as)
  (define jse (as-expr->js-source-element as))
  (if (as-expr->js-expr? as)
      jse
      `((function () ,jse))))

(define (as-expr->js-expr? as)
  (match as
    [`(begin ,fexpr ,rexpr ...)
     #t]
    [`(begin0 ,fexpr ,rexpr ...)
     #f]
    [`(set! ,id ,expr)
     #t]
    [`(lambda (,id ...) ,expr)
     #t]
    [`(local [,definition ...] ,expr)
     #f]
    [`(cond ,cexpr ...)
     #f]
    [`(case ,cexpr ...)
     #f]
    [`(if ,cexpr ,texpr ,fexpr)
     #f]
    [`(when ,cexpr ,bexpr)
     #f]
    [`(unless ,cexpr ,bexpr)
     #f]
    [`(and ,expr ...)
     #t]
    [`(or ,expr ...)
     #t]
    [`empty
     #t]    
    [(quote ,quoted)
     #t]
    [(quasiquote ,quasiquoted)
     #t]
    [`(,fexpr ,aexpr ...)
     #t]
    [`true
     #t]
    [`false
     #f]
    [(? symbol? id)
     ; XXX
     id]
    [(? number? n)
     n]
    [(? string? s)
     s]
    [(? char? c)
     c]))

(define (quote->expr q)
  (match q
    [(? symbol? s)
     ; XXX Symbols are strings
     (symbol->string s)]
    [(? number? n)
     n]
    [(? string? s)
     s]
    [(? char? c)
     c]
    [`(,q ...)
     `(list ,@(map quote->expr q))]))

(define (quasiquote->expr q)
  (match q
    [(? symbol? s)
     ; XXX Symbols are strings
     (symbol->string s)]
    [(? number? n)
     n]
    [(? string? s)
     s]
    [(? char? c)
     c]
    [`(,'unquote ,expr)
     expr]
    [`(,'unquote-splicing ,expr)
     (error 'quasiquote->expr "Invalid context")]
    [`(,q ...)
     `(append ,@(map quasiquote->exprs q))]))

(define (quasiquote->exprs q)
  (match q
    [`(,'unquote ,expr)
     `(list ,expr)]
    [`(,'unquote-splicing ,expr)
     expr]
    [`(,q ...)
     `(list (append ,@(map quasiquote->exprs q)))]
    [q
     `(list ,(quasiquote->expr q))]))

(define as-expr->js-substatement as-expr->js-source-element)
