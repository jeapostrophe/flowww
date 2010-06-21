#lang scheme
(require (planet byu-plt/flowww:1:0)
         "fwitter.ss"
         "view-fwitter.ss")

(provide;/contract
 make-start
 create-user follow-user! user-status-update! get-user
 )

;; start: request -> goes to login page
(define ((make-start) request)
  (show-login))

;; show-login-page
;; calls the render login with the appropiate data
(define (show-login)
  (let* ([login-handler 
          (lambda (request)
            (define username 
              (binding->string #"user" request))
            (printf "User Logging in: ~a~n" (get-user username))
            (show-profile (get-user username)))]
         
         [response-generator 
          (lambda (make-url)
            (render-login (make-url login-handler)))])
    
    (send/suspend/dispatch response-generator)))

;; show-profile-page: boolean User request -> void
;; calls the render-profile with the correct information 
(define (show-profile user)
  (let* ([update-handler 
          (flowww-handler (lambda (new-status)
                            (user-status-update! user new-status)))]
         
         [response-generator
          (lambda (make-url)            
            (render-profile (user-name user)
                            (value-now (user-status-b user))
                            (flowww-post (make-url update-handler)
                                         (field 
                                          ((field
                                            document 
                                            getElementById)
                                           "status")
                                          value))
                            (user-follow-page-b user)
                            flowww-script))])
    
    (send/suspend/dispatch response-generator)))