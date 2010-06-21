#lang scheme
(require xml)

(define-match-expander const
  (syntax-rules ()
    [(_ v p ...)
     (app (lambda _ v) p ...)]))
(define-match-expander xexpr-rest
  (syntax-rules ()
    [(_ tag attrs body)
     (list-rest 
      tag
      (or 
       ; If there are attributes it's easy, we just need to ensure they are attributes
       (list-rest (and (attributes _ (... ...))
                       attrs)
                  body)
       ; If not
       (and
        ; we have to make sure empty attrs are allowed
        (const empty attrs)
        ; and ensure we aren't missing attrs,
        (not (list-rest (attributes _ (... ...)) _))
        ; then continue
        body)))]))
(define-match-expander xexpr
  (syntax-rules ()
    [(_ tag attrs body ...)
     (xexpr-rest tag attrs (list body ...))]))
(define-match-expander attributes
  (syntax-rules ()
    [(_ attrp ...)
     (and (list (attr _ _) (... ...))
          (list-no-order attrp ...))]))
(define-match-expander attr
  (syntax-rules ()
    [(_ key value)
     (list (? symbol? key)
           (? string? value))]))

(define (xexpr-atom? x)
    (or (number? x) (symbol? x) (string? x) (cdata? x)))

(provide const xexpr xexpr-rest attributes attr xexpr-atom?)

;;;

(match `(body "Hey")
  [(xexpr 'body _ b ...)
   b])

(match `(body "Hey")
  [(xexpr 'body (attributes (attr k v) ...) b ...)
   (list k v)])

(match `(body ([id "foo"]) "Hey")
  [(xexpr 'body (attributes (attr k v) ...) b ...)
   (list k v)])

(match `(body ([id "foo"] [zog "bar"]) "Hey")
  [(xexpr 'body (attributes (attr 'zog zog) (attr 'id id)) b ...)
   (list zog id)])

(match `(ul . behavior)
  [(xexpr tag attrs body ...)
   (list tag attrs body)]
  [else
   #f])

(match `(ul . behavior)
  [(xexpr-rest tag attrs body)
   (list* tag attrs body)])