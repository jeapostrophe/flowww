#lang frtime
(require web-server/servlet-env
         (planet byu-plt/flowww:1:0)
         "view-2.ss"
         "model-2.ss"
         scheme/base)

;; Controller
(define (show-login)
  (let* ([reg-handler
          (lambda (request)
            (define username 
              (binding->string #"user" request))
            (define status
              (binding->string #"status" request))
            (printf "Registering name: ~a status: ~a~n" username status)
            (create-user! username status)
            (show-login))]
         
         [login-handler 
          (lambda (request)
            (define username 
              (binding->string #"user" request))
            (printf "Logging in name: ~a~n" username)
            (show-profile username))]
         [response-generator 
          (lambda (make-url)
            (render-login (make-url login-handler)
                          (make-url reg-handler)))])
    
    (printf "Showing Login~n")
    (send/suspend/dispatch response-generator)))


(define (show-profile username)
  (let* ([update-handler
          (flowww-handler
           (lambda (data)
             (printf "updating username: ~a postdata: ~a~n" username data)
             (user-status-update! username data)))]
         
         [follow-handler
          (flowww-handler
           (lambda (data)
             (printf "following username: ~a postdata: ~a~n" username data)
             (follow-user! username data)))]
         
         [response-generator
          (lambda (make-url)
            (render-profile username
                            (user-status-b username)
                            (flowww-post (make-url update-handler)
                                         (field
                                          ((field
                                            document
                                            getElementById)
                                           "status")
                                          value))
                            (flowww-post (make-url follow-handler)
                                         (field
                                          ((field
                                            document
                                            getElementById)
                                           "follow")
                                          value))
                            (user-list-b)
                            (user-follow-list-b username)
                            flowww-script))])
    
    (printf "Showing Profile: ~a~n" username)
    (send/suspend/dispatch response-generator)))


;; Run Server
(define ((make-start) request)
  (show-login))

(serve/frp (wrap-start (make-start)))