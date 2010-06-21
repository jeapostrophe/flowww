#lang frtime
(require "../../js/js.ss"
         web-server/http
         net/url
         web-server/http/bindings)
(provide start)

(define (start input-request-e)
  (input-request-e
   . ==> .
   (lambda (input-request)
     (define binds (request-bindings input-request))
     (cond
       [(exists-binding? 'mode binds)
        (cond
          [(string=? (extract-binding/single 'mode binds) "get-time")
           (number->string seconds)]
          [else
           (error 'start "Unknown mode~n")])]
       [else
        `(html (head (title "FrWeb/Flapjax Time Test")
                     (script ([type "text/javascript"] [src "/flapjax-impl.js"]) "")
                     ,(javascript
                       (function loader ()
                                 (var [oneSecE (timerE 1000)])
                                 (var [makeReq (function (t)
                                                         (return
                                                          (object [url "/servlets/standalone.ss"]
                                                                  [request "get"]
                                                                  [response "plain"]
                                                                  [fields (object [mode "get-time"])])))])
                                 (var [timeE
                                       (getWebServiceObjectE
                                        ((field oneSecE mapE)
                                          makeReq))])
                                 (var [timeB
                                       ((field timeE startsWith) "(null)")])
                                 (insertDomB
                                  (SPAN
                                   (object (id "time"))
                                   "The current time is: "
                                   timeB)
                                  "body"
                                  "beginning"))))
               (body ([id "body"] [onload ,(js-expr (loader))])
                     ""))]))))