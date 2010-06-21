#lang scheme

(require web-server/servlet-env
         (planet byu-plt/flowww:1:0)
         "view-1.ss"
         "model-1.ss"
         scheme/base)

;; Controller
(define (show-login)
  (let* ([reg-handler
          (lambda (request)
            (define username 
              (binding->string #"user" request))
            (define status
              (binding->string #"status" request))
            (create-user! username status)
            (show-login))]
         
         [login-handler 
          (lambda (request)
            (define username 
              (binding->string #"user" request))
            (show-profile username))]
         [response-generator 
          (lambda (make-url)
            (render-login (make-url login-handler)
                          (make-url reg-handler)))])
    
    (send/suspend/dispatch response-generator)))



(define (show-profile username)
  (let* ([update-handler
          (lambda (request)
            (define status
              (binding->string #"status" request))
            (user-status-update! username status)
            (show-profile username))]
         
         [follow-handler
          (lambda (request)
            (define follow
              (binding->string #"follow" request))
            (follow-user! username follow)
            (show-profile username))]
         
         [response-generator
          (lambda (make-url)
            (render-profile username
                            (user-status username)
                            (make-url update-handler)
                            (make-url follow-handler)
                            (user-list)
                            (user-follow-list username)))])
    
    (send/suspend/dispatch response-generator)))



;; Run Server
(define ((make-start) request)
  (show-login))

(serve/servlet (wrap-start (make-start)))