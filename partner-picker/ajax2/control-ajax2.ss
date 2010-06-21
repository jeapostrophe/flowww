#lang scheme
(require web-server/servlet
         "../../js/js.ss"
         "../original/model.ss"
         "view-ajax2.ss")
(provide/contract
 [make-start (picker? . -> . (request? . -> . response/c))])

;; change this value to adjust the number of team that are allowed to be made
(define num-of-teams 3)

;; start: request -> goes to login page
(define ((make-start picker) request)
  (show-login #:picker picker))

;; show-login-page: picker String -> void
;; calls the render login with the appropiate data
(define (show-login #:picker picker 
                    #:error [err-msg #f])
  (local [(define (response-generator make-url)
            (render-login #:submit-url (make-url login-handler)
                          #:new-profile-url (make-url new-profile-handler)
                          #:error err-msg))
          
          (define (login-handler request)
            (define username 
              (binding->string #"e-mail" request))
            (define password
              (binding->string #"password" request))
            (with-handlers 
                ([exn:fail? 
                  (lambda (exn)      
                    (redirect/get)
                    ; XXX Make error better for users
                    (show-login #:picker picker 
                                #:error (exn-message exn)))]);"Invalid username or password."))])
              (show-profile #:picker picker
                            #:user (user-login picker username password))))
          
          (define (new-profile-handler request)
            (show-new-profile #:picker picker))]
    
    (send/suspend/dispatch response-generator)))

;; show-new-profile-page: request -> void
;; calls the render-new-profile with the needed information
(define (show-new-profile #:picker picker 
                          #:error [err-msg #f])
  (local [(define (response-generator make-url)
            (render-new-profile #:submit-url (make-url availability-handler)
                                #:error err-msg))
          
          (define (availability-handler request)
            (define first-name
              (binding->string #"firstname" request))
            (define last-name
              (binding->string #"lastname" request))
            (define e-mail 
              (binding->string #"e-mail" request))
            (define password
              (binding->string #"password" request))
            (define section
              (binding->string #"section" request))
            
            (with-handlers 
                ([exn:fail? 
                  (lambda (exn)                 
                    (redirect/get)
                    (show-new-profile #:picker picker
                                      #:error (exn-message exn);"One of the fields was filled incorrectly"
                                      ))])
              (show-profile #:picker picker
                                 #:user (add-user! picker
                                                   first-name 
                                                   last-name 
                                                   e-mail 
                                                   password 
                                                   (string->number section)
                                                   empty))))]
    
    (send/suspend/dispatch response-generator)))

(require tests/eli-tester)
(define (quote-string s)
  (regexp-replace* "%22" s "%5C%22"))

(define (escape-string s)
  (regexp-replace* "%20" s " "))
  

;; show-profile-page: boolean User request -> void
;; calls the render-profile with the correct information 
(define (show-profile #:picker picker
                      #:error [err-msg #f] 
                      #:user user)
  (local [(define (response-generator make-url)
            (render-profile #:availability-url (javascript: (getXmlHttpObject 
                                                             #,(quote-string 
                                                                 (make-url availability-handler))))
                            #:error err-msg
                            #:firstname (user-first-name user)
                            #:lastname (user-last-name user)
                            #:section (user-section user)
                            #:email (user-email user)
                            #:requests-url (javascript: (getXmlHttpObject
                                                         #,(quote-string
                                                            (make-url requests-handler))))
                            #:onload (javascript: (getXmlHttpObject
                                                         #,(escape-string
                                                            (make-url requests-handler))))
                            #:script script))
          
          (define script
            (javascript
             (var xmlHttp)
             (function stateChangeHandler () 
                       (if (== (field xmlHttp readyState) 4)
                           (if (== (field xmlHttp status) 200)
                               (block
                                ;(alert (+ "response:\n" (field xmlHttp responseText)))
                                (= (field [(field document getElementById) "ajax"] innerHTML)
                                   (field xmlHttp responseText)))
                               (alert (+ "Bad request status: " (field xmlHttp status))))))
             (function getXmlHttpObject (url)
                       (try 
                        (block
                         (= xmlHttp (new XMLHttpRequest)))
                        (catch exn
                          (block
                           (try
                            (block
                             (= xmlHttp (new ActiveXObject "Msxml2.XMLHTTP")))
                            (catch exn
                              (block
                               (try
                                (block
                                 (= xmlHttp (new ActiveXObject "Microsoft.XMLHTTP")))
                                (catch exn
                                  (block
                                   (alert "Unable to create an XMLHTTP instance"))))))))))
                       (= (field xmlHttp onreadystatechange)
                          stateChangeHandler)
                       ;(alert (+ "url before:\n" url))
                       ((field xmlHttp open) "GET" url #t)
                       ;(alert (+ "url after:\n" url))
                       ((field xmlHttp send) null))))
          
          (define (requests-handler request)
            (with-handlers
                ([exn:fail?
                  (lambda (exn)
                    (show-profile #:error (exn-message exn);"Internal error unable to show requests at this time"
                                  #:picker picker
                                  #:user user))])
              (redirect/get)
              (show-requests #:picker picker
                             #:user user)))
          
          (define (availability-handler request)
            (with-handlers 
                ([exn:fail?
                  (lambda (exn)                                   
                    (show-profile #:error (exn-message exn);"Internal error unable to show availability at this time"
                                  #:picker picker
                                  #:user user))])
              (show-availability #:picker picker
                                 #:user user)))
          
          (define (request-status-handler info)
            (lambda (request)
              (with-handlers 
                  ([exn:fail?
                    (lambda (exn)                                  
                      (show-profile #:error (exn-message exn);"Unable to change request status."
                                    #:picker picker
                                    #:user user))])
                (show-profile #:picker picker
                              #:user user
                              #:view "requests"))))
          
          (define (change-time-handler info)
            (lambda (request)
              (with-handlers 
                  ([exn:fail?
                    (lambda (exn)                                    
                      (show-availability #:error (exn-message exn);"Unable to change times specified."
                                         #:picker picker
                                         #:user user))])
                (change-available-time! user (floor (/ (second info) 24)) (modulo (second info) 24))
                (show-profile #:picker picker
                              #:user user
                              #:view "availability"))))]  
    (send/suspend/dispatch response-generator)))

;; show-request-page: boolean user request -> void
;; calls render-requests with the correct information
(define (show-requests #:picker picker
                       #:error [err-msg #f]
                       #:user user)
  (local [(define (response-generator make-url)
            (render-requests
             #:make-submit-request-url (lambda (info)
                                         (javascript: (getXmlHttpObject
                                                       #,(quote-string
                                                          (make-url 
                                                           (submit-request-handler info))))))
             #:make-accept-request-url (lambda (info)
                                         (javascript: (getXmlHttpObject
                                                       #,(quote-string
                                                          (make-url 
                                                           (accept-request-handler info))))))
             #:team-data (make-team-table user)
             #:pending-data (make-pending-table user)
             #:request-data (make-request-table user)
             #:candidate-data (make-candidate-table user)
             #:error err-msg))
          
          (define (submit-request-handler info)
            (lambda (request)
              (with-handlers 
                  ([exn:fail?
                    (lambda (exn)                                    
                      (show-requests #:error (exn-message exn);"Unable to make specified request."
                                     #:picker picker
                                     #:user user))])
                (request-partner! user (second info) (first info))
                (redirect/get)
                (show-requests #:picker picker
                               #:user user))))
          
          (define (accept-request-handler info)
            (lambda (request)
              (with-handlers 
                  ([exn:fail?
                    (lambda (exn)                                    
                      (show-requests #:error (exn-message exn);"Unable to accept specified request."
                                     #:picker picker
                                     #:user user))])
                (accept-request! user (second info) (third info))
                (remove-request! user (third info))
                (remove-request! (second info) (third info))                
                (redirect/get)
                (show-requests #:picker picker
                               #:user user))))]
    
    (send/suspend/dispatch response-generator)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-team-table user)
  (define times (user-availability user))
  (map (lambda (user-list)
         (list (user-first-name (first user-list))
               (user-last-name (first user-list))
               (length (compare-availability user (first user-list)))
               (second user-list))) 
       (user-partners user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-pending-table user)
  (define times (user-availability user))
  (map (lambda (user-list)
         (list (user-first-name (first user-list))
               (user-last-name (first user-list))
               (length (compare-availability user (first user-list)))
               (second user-list))) 
       (user-pendings user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns requests made of the user formatted for a table
(define (make-request-table user)
  (define times (user-availability user))
  (map (lambda (user-number-list)
         (list
          (user-first-name (first user-number-list))
          (user-last-name (first user-number-list))
          (length (compare-availability user (first user-number-list)))
          (second user-number-list)
          (cons "Accept" user-number-list))) 
       (user-requests user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-candidate-table user)
  (define times (user-availability user))
  (map (lambda (usr)
         (list
          (user-first-name usr)
          (user-last-name usr)
          (length (compare-availability user usr))
          (build-list num-of-teams (lambda (x) (list (add1 x) usr))))) 
       (sort-availability user (user-candidates user))))

;; show-availability-page: boolean user request -> void
;; calls render-availability withthe correct information
(define (show-availability #:picker picker
                           #:error [err-msg #f]
                           #:user user)
  (local [(define (response-generator make-url)
            (render-availability
             #:make-url (lambda (info)
                          (javascript: (getXmlHttpObject
                                        #,(quote-string
                                           (make-url 
                                            (change-time-handler info))))))
             #:availability-data (make-availability-table user)
             #:error err-msg))
          
          (define (change-time-handler info)
            (lambda (request)
              (with-handlers 
                  ([exn:fail?
                    (lambda (exn)                                    
                      (show-availability #:error (exn-message exn);"Unable to change times specified."
                                         #:picker picker
                                         #:user user))])
                (change-available-time! user (floor (/ (second info) 24)) (modulo (second info) 24))
                (redirect/get)
                (show-availability #:picker picker
                                   #:user user))))]
    
    (send/suspend/dispatch response-generator)))

;; make-availablility-table : user -> (listof list)
;; takes a user and returns a list of list of string number
;; where the string tells if user is free or busy for the hour number
(define (make-availability-table user)
  (define times (user-availability user))
  (build-list 24 
              (lambda (hour) 
                (cons (format-hour hour)
                      (build-list 7
                                  (lambda (day)
                                    (if (member (+ hour (* day 24)) times)
                                        (list "Free" (+ hour (* day 24)))
                                        (list "Busy" (+ hour (* day 24))))))))))

(define (format-hour hour)
  (if (> 12 hour)
      (if (equal? 0 hour)
          "12:00 am"
          (format "~a:00 am" hour))
      (if (equal? 12 hour)
          "12:00 pm"
          (format "~a:00 pm" (- hour 12)))))

;; binding->string: id request -> String
;; throws an error if binding was not recieved
(define (binding->string id request)
  (define binds (request-bindings/raw request))
  (match (bindings-assq id binds)
    [(? binding:form? b)
     (bytes->string/utf-8
      (binding:form-value b))]
    [_
     (error 'binding->string "Binding not received")]))
