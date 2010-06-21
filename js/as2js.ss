#lang scheme
(require (planet dherman/javascript:9:0/ast)
         (planet dherman/javascript:9:0/parse))

;; Help
(define-syntax try-in-order
  (syntax-rules ()
    [(_ e)
     e]
    [(_ fe re ...)
     (with-handlers ([exn:fail?
                      (lambda _
                        (try-in-order
                         re ...))])
       fe)]))

;; Compiler

; syntax -> region
(define (syntax->location stx)
  stx)

; syntax (of identifier) -> Identifier
(define (id->Identifier stx)
  ; XXX Mangle
  (make-Identifier (syntax->location stx) (syntax->datum stx)))

; syntax -> (listof SourceElement?)
(define (program->SourceElements stx)
  (syntax-case stx ()
    [(def-or-expr ...)
     (append def-or-expr->SourceElements
             (syntax->list #'(def-or-expr ...)))]))

; syntax -> (listof SourceElement?)
(define (def-or-expr->SourceElements stx)
  (try-in-order
   (definition->SourceElements stx)
   (expr->SourceElements stx)
   (test-case->SourceElements stx)
   (library-require->SourceElements stx)))

; syntax -> (listof SourceElement?)
(define (definition->SourceElements stx)
  (syntax-case stx (define define-struct)
    [(define (func-id arg-id ...)
       expr)
     (list
      (make-FunctionDeclaration
       (syntax->location stx)
       (id->Identifier #'func-id)
       (map id->Identifier (syntax->list #'(arg-id ...)))
       (expr->SourceElements #'expr)))]
    [(define val-id expr)
     (list
      (make-VariableDeclaration
       (syntax->location stx)
       (list (make-VariableInitializer
              (syntax->location stx)
              (id->Identifier #'val-id)
              (expr->Expression #'expr)))))]
    [(define-struct struct-id (field-id ...))
     (with-syntax ([make-struct-id (datum->syntax stx (string->symbol (format "make-~a" (syntax->datum #'struct-id))) stx)]
                   [struct-label (datum->syntax stx (gensym (syntax->datum #'struct-id)) stx)]
                   [struct-id? (datum->syntax stx (string->symbol (format "~a?" (syntax->datum #'struct-id))) stx)]
                   [(struct-id-field-id ...)
                    (map (lambda (field-id)
                           (datum->syntax stx 
                                          (string->symbol (format "~a-~a" 
                                                                      (syntax->datum #'struct-id)
                                                                      (syntax->datum field-id)))
                                          stx))
                         (syntax->list #'(field-id ...)))]
                   [(field-id-num ...)
                    (map (lambda (num)
                           (datum->syntax stx num stx))
                         (build-list 
                          (length
                           (syntax->list #'(field-id ...)))
                          (lambda (x) x)))]
                   [(set-struct-id-field-id! ...)
                    (map (lambda (field-id)
                           (datum->syntax stx 
                                          (string->symbol (format "set-~a-~a!" 
                                                                      (syntax->datum #'struct-id)
                                                                      (syntax->datum field-id)))
                                          stx))
                         (syntax->list #'(field-id ...)))])
       (append 
        (map definition->SourceElements
             (syntax->list
              (syntax/loc stx
                ((define (make-struct-id field-id ...)
                   (vector 'struct-label
                           field-id ...))
                 (define (struct-id? val)
                   (and (vector? val)
                        (equal? (vector-ref val 0) 'struct-label)))
                 (define (struct-id-field-id the-struct)
                   (if (struct-id? the-struct)
                       (vector-ref the-struct field-id-num)
                       (error 'struct-id-field-id "Must be passed struct of proper type (XXX)")))
                 ...
                 (define (set-struct-id-field-id! the-struct val)
                   (if (struct-id? the-struct)
                       (vector-set! the-struct field-id-num val)
                       (error 'set-struct-id-field-id! "Must be passed struct of proper type (XXX)")))
                 ...))))))]))

; syntax -> (listof SourceElement)
(define (expr->SourceElements stx)
  (list (expr->SourceElement stx)))

; Expression -> SourceElement
(define (Expression->SourceElement e)
  (make-ExpressionStatement
   (ast-location e)
   e))

; SourceElement -> Expression
(define (SourceElement->Expression se)
  (make-CallExpression
   (Term-location se)
   (make-FunctionExpression
    (Term-location se)
    #f empty
    (list se))
   empty))

; syntax -> SourceElement
(define (expr->SourceElement stx)
  (syntax-case stx (begin begin0 set! lambda local letrec let let* recur cond else case if when unless and quote quasiquote)
    [(begin fe re ...)
     (make-BlockStatement
      (syntax->location stx)
      (map expr->SourceElement ; XXX Should be expr->SubStatement
           (syntax->list #'(fe re ...))))]
    [(begin0 fe re ...)
     (make-BlockStatement
      (syntax->location stx)
      (list* (make-VariableDeclaration
              (syntax->location #'fe)
              (list (make-VariableInitializer
                     (syntax->location #'fe)
                     (id->Identifier (datum->syntax #'fe (gensym 'begin0-tmp)))
                     (expr->Expression #'fe))))
             (map expr->SourceElement ; XXX Should be expr->SubStatement
                  (syntax->list #'(re ...)))))]
    [(set! v e)
     (Expression->SourceElement
      (expr->Expression stx))]
    [(lambda (arg-id ...) expr)
     (Expression->SourceElement
      (expr->Expression stx))]
    [(local (definition ...) expr)
     (make-BlockStatement
      (syntax->location stx)
      (append (apply append
                     (map definition->SourceElements
                          (syntax->list #'(definition ...))))
              (list
               (expr->SourceElement #'expr))))]
    [(let ([id id-expr] ...) body-expr)
     ; XXX Create a block
     (Expression->SourceElement
      (expr->Expression stx))]
    ; XXX
    ))
    
; syntax -> Expression
(define (expr->Expression stx)
  (syntax-case stx (begin begin0 set! lambda local letrec let let* recur cond else case if when unless and quote quasiquote)
    [(begin fe re ...)
     (SourceElement->Expression
      (expr->SourceElement stx))]
    [(begin0 fe re ...)
     (SourceElement->Expression
      (expr->SourceElement stx))]
    [(set! v e)
     (make-AssignmentExpression
      (syntax->location stx)
      (make-VarReference (syntax->location #'v) (id->Identifier #'v))
      (expr->Expression #'e))]
    [(lambda (arg-id ...) expr)
     (make-FunctionExpression
      (syntax->location stx)
      #f (map id->Identifier (syntax->list #'(arg-id ...)))
      (expr->SourceElements #'expr))]
    [(local (definition ...) expr)
     (SourceElement->Expression
      (expr->SourceElement stx))]
    [(let ([id id-expr] ...) body-expr)
     (make-CallExpression
      (syntax->location stx)
      (make-FunctionExpression
       (syntax->location stx)
       #f (map id->Identifier (syntax->list #'(id ...)))
       (expr->SourceElements #'body-expr))
      (map expr->Expression (syntax->list #'(id-expr ...))))]     
    ; XXX
    ))
      
(define (test-case->SourceElements stx)
  (error 'test-case "Not implemented XXX"))

(define (library-require->SourceElements stx)
  (error 'library-require "Not implemented XXX"))