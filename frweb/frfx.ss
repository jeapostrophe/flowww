#lang scheme
(require web-server/servlet
         web-server/servlet-env
         scheme/runtime-path
         xml
         "xexpr.ss"
         "../js/js.ss"
         (only-in frtime/frtime
                  lift-strict
                  event-receiver
                  event? behavior? hold
                  raise-reactivity send-synchronous-event
                  deep-value-now))

;;;

(define (behavior-id bi) (format "behavior~a" bi))

(define (create-debehaviorize)
  (define BEHAVIORS (make-hasheq))
  (define last-behavior 0)
  (define (remember-behavior! b)
    (begin0 last-behavior
            (hash-set! BEHAVIORS last-behavior b)
            (set! last-behavior (add1 last-behavior))))
  (define (behavior-grab bi)
    ;; XXX Must be xexpr
    (deep-value-now (hash-ref BEHAVIORS bi)))
  (define (maybe-map f l-or-b)
    (match l-or-b
      [(list)
       empty]
      [(cons e r)
       (cons (f e) (maybe-map f r))]
      [(? behavior? b)
       ; XXX This should communicate with debehaviorize
       (list (f (lift-strict (curry list* 'div) b)))]))
  
  (define (debehaviorize x)
    #;(printf "De: ~S~n" x)
    ;; XXX Must be xexpr
    (match x
      [(? behavior? b)
       (define bi (remember-behavior! b))
       `(span ([id ,(behavior-id bi)]) "")]
      [(xexpr-rest tag attrs body)
       (list* tag attrs
              (maybe-map debehaviorize body))]
      [(? xexpr-atom? a)
       a]))
  
  (define (insert-behaviors behavior-url xe)
    (match xe
      [(xexpr-rest 'html html-attrs
                   (list-no-order
                    (xexpr 'head head-attrs head-rst ...)
                    (xexpr 'body (attributes attrs ...)
                           rest ...)))
       `(html ,html-attrs
              (head ,head-attrs ,@head-rst
                    (script ([type "text/javascript"] [src "/flapjax-impl.js"]) "")
                    ,(javascript
                      (var [howOftenE (timerE 1000)])
                      (function getRealBehavior (i)
                                (var [makeReq (function (t)
                                                        (return
                                                         (object [url #,behavior-url]
                                                                 [request "get"]
                                                                 [response "plain"]
                                                                 [fields (object [behavior i])])))])
                                (var [valueE
                                      (getWebServiceObjectE
                                       (howOftenE.mapE makeReq))])
                                (return valueE))
                      (var [behaviors
                            (array
                             #,@(for/list ([i (in-range last-behavior)])
                                  #`(getRealBehavior #,(number->string i))))])
                      (function getBehavior (i)
                                (return (field-ref behaviors i)))
                      (function insertBehavior (i id init)
                                (var [valueE (getBehavior i)])
                                (var [valueB (valueE.startsWith init)])
                                (insertDomB valueB id "over" #t))
                      (function loader ()
                                #,@(for/list ([i (in-range last-behavior)])
                                     #`(insertBehavior #,i
                                                       #,(behavior-id i)
                                                       #,(xexpr->string (behavior-grab i)))))))
              (body ([id "body"] [onload ,(js-expr (loader))] . ,attrs)
                    . ,rest))]
      [(xexpr 'html html-attrs
              (xexpr 'body (attributes attrs ...)
                     rest ...))
       (insert-behaviors behavior-url
                         `(html ,html-attrs
                                (head)
                                (body ,attrs
                                      ,@rest)))]))
  
  (define (debehaviorize-top behavior-url b)
    (insert-behaviors behavior-url (debehaviorize b)))
  
  (define (behavior-snapshot req)
    (send/back
     (behavior-grab 
      (string->number 
       (extract-binding/single
        'behavior
        (request-bindings req))))))
  
  (values behavior-snapshot debehaviorize-top))

(define ((make-frp:send/back send/back) r)
  (define-values (behavior-snapshot debehaviorize-top) (create-debehaviorize))
  (behavior-snapshot (send/suspend (lambda (k-url) (send/back (debehaviorize-top k-url r))))))
(define frp:send/back (make-frp:send/back send/back))
(define frp:send/finish (make-frp:send/back send/finish)) ; XXX Could/Should use send/forward
(define ((make-frp:send/suspend send/suspend) gen-r)
  (define-values (behavior-snapshot debehaviorize-top) (create-debehaviorize))
  (send/suspend/dispatch
   (lambda (embed/url)
     (debehaviorize-top 
      (embed/url behavior-snapshot)
      (gen-r (embed/url (lambda (x) x)))))))
(define frp:send/suspend (make-frp:send/suspend send/suspend))
(define frp:send/suspend/url (make-frp:send/suspend send/suspend/url))
(define frp:send/forward (make-frp:send/suspend send/forward))
(define ((make-frp:send/suspend/dispatch send/suspend/dispatch) gen-r)
  (define-values (behavior-snapshot debehaviorize-top) (create-debehaviorize))
  (send/suspend/dispatch
   (lambda (embed/url)
     (debehaviorize-top 
      (embed/url behavior-snapshot)
      (gen-r embed/url)))))
(define frp:send/suspend/dispatch (make-frp:send/suspend/dispatch send/suspend/dispatch))
(define frp:send/suspend/url/dispatch (make-frp:send/suspend/dispatch send/suspend/url/dispatch))

(require
 web-server/http
 web-server/http/bindings)
(provide
 serve/frp
 wrap-start
 flowww-home
 (all-from-out web-server/http
               web-server/http/bindings)
 (rename-out [frp:send/back send/back]
             [frp:send/finish send/finish]
             [frp:send/suspend send/suspend]
             [frp:send/suspend/url send/suspend/url]
             [frp:send/forward send/forward]
             [frp:send/suspend/dispatch send/suspend/dispatch]
             [frp:send/suspend/url/dispatch send/suspend/url/dispatch]))

(define (wrap-start s)
  (define (start input-request)
    (frp:send/back (s input-request)))
  start)

(define-runtime-path flowww-home "../flapjax/fx")
(define (serve/frp s)
  (serve/servlet (wrap-start s)
                 #:extra-files-paths
                 (list flowww-home)))