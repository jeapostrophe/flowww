#lang scheme
(require web-server/servlet
         file/md5
         scheme/async-channel
         "../../js/js.ss"
         "../comet/kill-safe.ss"
         "../original/model.ss"
         "../ajax2/view-ajax2.ss")
(provide;/contract
 make-start ;(picker? . -> . (request? . -> . response/c))]
 )

;; change this value to adjust the number of team that are allowed to be made
(define num-of-teams 3)

;; Server side polling server
(define-struct update-request (user old-data))
(define-struct server-state (update-time cache))
(define polling-server
  (kill-safe-server #:accept-new-requests? 
                    (lambda (st) #t)
                    #:handle-request-evt 
                    (lambda (st ur)
                      (guard-evt
                       (lambda ()
                         (if (waiting? st ur)
                             never-evt
                             (wrap-evt always-evt
                                       (lambda (_)
                                         (printf "~S done waiting~n" ur)
                                         (make-state+data st 'ok)))))))
                    #:register-new-request
                    (lambda (st ur) st)
                    #:register-response-delivery
                    (lambda (st ur) st)
                    #:register-request-failure
                    (lambda (st ur) st)
                    #:register-response-failure
                    (lambda (st ur) st)
                    #:state-change-evt
                    (lambda (st)
                      (wrap-evt db-change-notify-ch
                                (lambda (update-time)
                                  ;conversion to mzscheme
                                  #;(match st
                                      [(struct server-state (last-update cache))
                                       (if (<= update-time last-update)
                                           st
                                           (make-server-state update-time (make-hash)))])
                                  (cond
                                    [(server-state? st)
                                     (if (<= update-time (server-state-update-time st))
                                         st
                                         (make-server-state (server-state-update-time st) (make-hash)))]))))
                    #:initial-state
                    (make-server-state 0 (make-hash))))

(define db-change-notify-ch
  (make-async-channel))
(define (db-changed!)
  (async-channel-put db-change-notify-ch (current-seconds)))

(define (user-hash user)
  (let ([o (open-output-bytes)])
    (write (list (make-team-table user)
                 (make-pending-table user)
                 (make-request-table user)
                 (make-candidate-table user))
           o)
    (md5 (get-output-bytes o))))

(define (waiting? st req)
  #;(match req
      [(struct update-request (user old-data))
       (bytes=? old-data
                (hash-ref! (server-state-cache st) (user-email user)
                           (lambda () (user-hash user))))])
  (cond
    [(update-request? req)
     (bytes=? (update-request-old-data req)
              (hash-ref! (server-state-cache st) (user-email (update-request-user req))
                         (lambda () (user-hash (update-request-user req)))))]))

; wait-for-update : user db/hash -> void
(define (wait-for-update user old-data)
  (kill-safe-request 
   polling-server 
   (make-update-request user old-data))
  (void))

;; start: request -> goes to login page
(define ((make-start picker) request)
  (show-login picker))

;; show-login-page: picker String -> void
;; calls the render login with the appropiate data
(define (show-login picker) 
  ;err-msg #f)
  (let* ([new-profile-handler 
            (lambda (request)
              (show-new-profile picker))]
           
           [login-handler 
            (lambda (request)
              (define username 
                (binding->string #"e-mail" request))
              (define password
                (binding->string #"password" request))
              (with-handlers 
                  ([exn? 
                    (lambda (exn)      
                      (redirect/get)
                      ; XXX Make error better for users
                      (show-login picker 
                                  #;(exn-message exn)))]);"Invalid username or password."))])
                (show-profile picker
                              (user-login picker username password))))]
           
           [response-generator 
            (lambda (make-url)
              (render-login (make-url login-handler)
                            (make-url new-profile-handler)))]
           ;#:error err-msg))
           )
    
    (send/suspend/dispatch response-generator)))

;; show-new-profile-page: request -> void
;; calls the render-new-profile with the needed information
(define (show-new-profile picker) 
  ;#:error [err-msg #f])
  (let* ([availability-handler 
          (lambda (request)
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
                ([exn? 
                  (lambda (exn)                 
                    (redirect/get)
                    (show-new-profile picker
                                      ;#:error (exn-message exn);"One of the fields was filled incorrectly"
                                      ))])
              (show-profile picker
                            (add-user! picker
                                       first-name 
                                       last-name 
                                       e-mail 
                                       password 
                                       (string->number section)
                                       (list)))))]
         
         [response-generator 
          (lambda (make-url)
            (render-new-profile (make-url availability-handler)))
          ;#:error err-msg))
         ])
    
    (send/suspend/dispatch response-generator)))

(require tests/eli-tester)
(define (quote-string s)
  (regexp-replace* "%22" s "%5C%22"))

(define (escape-string s)
  (regexp-replace* "%20" s " "))


;; show-profile-page: boolean User request -> void
;; calls the render-profile with the correct information 
(define (show-profile picker
                      ;#:error [err-msg #f] 
                      user)
  (let* ([script
            (javascript
             (var xmlHttp)
             (function getXmlRequests (initUrl waitUrl)
                       (getXmlHttpObjectInit initUrl waitUrl))
             
             (function getXmlAvailability (url)
                       (getXmlHttpObject url))
             
             (function initialize (initUrl waitUrl)
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
                       (getXmlHttpObjectInit initUrl waitUrl))
             
             (function getXmlHttpObject (url)
                       ((field xmlHttp abort))
                       (= (field xmlHttp onreadystatechange)                          
                          (function () 
                                    (if (== (field xmlHttp readyState) 4)
                                        (if (== (field xmlHttp status) 200)
                                            (block
                                             (= (field [(field document getElementById) "ajax"] innerHTML)
                                                (field xmlHttp responseText)))))))
                       ((field xmlHttp open) "GET" url #t)
                       ((field xmlHttp send) null))
             
             (function getXmlHttpObjectRec (url)
                       ((field xmlHttp abort))
                       (= (field xmlHttp onreadystatechange)                          
                          (function () 
                                    (if (== (field xmlHttp readyState) 4)
                                        (if (== (field xmlHttp status) 200)
                                            (block
                                             (= (field [(field document getElementById) "ajax"] innerHTML)
                                                (field xmlHttp responseText))
                                             (getXmlHttpObjectRec url))))))
                       ((field xmlHttp open) "GET" url #t)
                       ((field xmlHttp send) null))
             
             (function getXmlHttpObjectInit (initUrl waitUrl)
                       ((field xmlHttp abort))
                       (= (field xmlHttp onreadystatechange)
                          (function ()
                                    (if (== (field xmlHttp readyState) 4)
                                        (if (== (field xmlHttp status) 200)
                                            (block
                                             (= (field [(field document getElementById) "ajax"] innerHTML)
                                                (field xmlHttp responseText))
                                             (getXmlHttpObjectRec waitUrl))))))
                       ((field xmlHttp open) "GET" initUrl #t)
                       ((field xmlHttp send) null)))]
          ;;end of Javascript
          
          [requests-handler 
           (lambda (request)
            (with-handlers
                ([exn?
                  (lambda (exn)
                    (show-profile ;#:error (exn-message exn);"Internal error unable to show requests at this time"
                     picker
                     user))])
              (redirect/get)
              (show-requests picker
                             user
                             (user-hash user))))]
          
          [init-requests-handler 
           (lambda (request)
            (with-handlers
                ([exn?
                  (lambda (exn)
                    (show-profile ;#:error (exn-message exn);"Internal error unable to show requests at this time"
                     picker
                     user))])
              (redirect/get)              
              (show-requests picker
                             user
                             #f)))]
          
          [availability-handler 
           (lambda (request)
            (with-handlers 
                ([exn?
                  (lambda (exn)                                   
                    (show-profile ;#:error (exn-message exn);"Internal error unable to show availability at this time"
                     picker
                     user))])
              (redirect/get)
              (show-availability picker
                                 user)))]
          
          [request-status-handler 
           (lambda (info)
            (lambda (request)
              (with-handlers 
                  ([exn?
                    (lambda (exn)                                   
                      (show-profile ;#:error (exn-message exn);"Unable to change request status."
                       picker
                       user))])
                (redirect/get)
                (show-profile picker
                              user
                              #:view "requests"))))]
          
          [response-generator 
           (lambda (make-url)
            (render-profile (javascript: (getXmlAvailability
                                          #,(quote-string 
                                             (make-url availability-handler))))
                            (javascript: (getXmlRequests
                                          #,(quote-string
                                             (make-url init-requests-handler))
                                          #,(quote-string
                                             (make-url requests-handler))))
                            (javascript: (initialize
                                          #,(escape-string
                                             (make-url init-requests-handler))
                                          #,(escape-string
                                             (make-url requests-handler))))
                            ;#:error err-msg
                            (user-first-name user)
                            (user-last-name user)
                            (user-section user)
                            (user-email user)                           
                            script))]) 
    
    (send/suspend/dispatch response-generator)))

;; show-request-page: boolean user request -> void
;; calls render-requests with the correct information
(define (show-requests picker
                       ;#:error [err-msg #f]
                       user
                       data-rendered)
  (when data-rendered
    (printf "waiting for ~S profile.~n" (user-first-name user))
    (wait-for-update user data-rendered))
  (printf "showing requests for ~S.~n" (user-first-name user))
  (let* ([requests-handler 
          (lambda (info)
            (lambda (request)
              (with-handlers
                  ([exn?
                    (lambda (exn)
                      (show-profile ;#:error (exn-message exn);"Internal error unable to show requests at this time"
                       picker
                       user))])
                (redirect/get)
                (show-requests picker
                               user
                               (user-hash user)))))]
          
          [submit-request-handler 
           (lambda (info)
            (lambda (request)
              (with-handlers 
                  ([exn?
                    (lambda (exn) 
                      (show-requests ;#:error (exn-message exn);"Unable to make specified request."
                       picker
                       user
                       #f))])
                (request-partner! user (cdar info) (car info))
                (db-changed!)
                (redirect/get)
                (show-requests picker
                               user
                               #f))))]
          
          [accept-request-handler 
           (lambda (info)
            (lambda (request)
              (with-handlers 
                  ([exn?
                    (lambda (exn) 
                      (show-requests ;#:error (exn-message exn);"Unable to accept specified request."
                       picker
                       user
                       #f))])
                (accept-request! user (cdar info) (cddar info))
                (remove-request! user (cddar info))
                (remove-request! (cdar info) (cddar info))
                (db-changed!)
                (redirect/get)
                (show-requests picker
                               user
                               #f))))]
          
          [response-generator 
           (lambda (make-url)
            (render-requests
             (lambda (info)
               (javascript: (getXmlRequests
                             #,(quote-string
                                (make-url 
                                 (accept-request-handler info)))
                             #,(quote-string
                                (make-url 
                                 (requests-handler info))))))
             (lambda (info)
               (javascript: (getXmlRequests
                             #,(quote-string
                                (make-url 
                                 (submit-request-handler info)))
                             #,(quote-string
                                (make-url 
                                 (requests-handler info))))))
             
             (make-team-table user)
             (make-pending-table user)
             (make-request-table user)
             (make-candidate-table user)))
          ;#:error err-msg))
           ])
    
    (send/suspend/dispatch response-generator)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-team-table user)
  (define times (user-availability user))
  (map (lambda (user-list)
         (list (user-first-name (car user-list))
               (user-last-name (car user-list))
               (length (compare-availability user (car user-list)))
               (cdar user-list))) 
       (user-partners user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-pending-table user)
  (define times (user-availability user))
  (map (lambda (user-list)
         (list (user-first-name (car user-list))
               (user-last-name (car user-list))
               (length (compare-availability user (car user-list)))
               (cdar user-list))) 
       (user-pendings user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns requests made of the user formatted for a table
(define (make-request-table user)
  (define times (user-availability user))
  (map (lambda (user-number-list)
         (list
          (user-first-name (car user-number-list))
          (user-last-name (car user-number-list))
          (length (compare-availability user (car user-number-list)))
          (cdar user-number-list)
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
(define (show-availability picker
                           ;#:error [err-msg #f]
                           user)
  (let* ([change-time-handler 
          (lambda (info)
            (lambda (request)
              (with-handlers 
                  ([exn?
                    (lambda (exn)                                    
                      (show-availability ;#:error (exn-message exn);"Unable to change times specified."
                       picker
                       user))])
                (change-available-time! user (floor (/ (cdar info) 24)) (modulo (cdar info) 24))
                (db-changed!)
                (redirect/get)
                (show-availability picker
                                   user))))]
          
          [response-generator 
           (lambda (make-url)
            (render-availability
             (lambda (info)
               (javascript: (getXmlHttpObject
                             #,(quote-string
                                (make-url 
                                 (change-time-handler info))))))
             (make-availability-table user)))
          ;#:error err-msg))
          ])
    
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
