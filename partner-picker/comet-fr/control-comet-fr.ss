#lang frtime
(require "../../frweb/frfx.ss"
         (lifted web-server/servlet
                 redirect/get
                 #;request-bindings/raw
                 #;bindings-assq
                 #;binding:form-value
                 #;binding:form?)
         file/md5
         mzlib/async-channel
         (only scheme/base
               never-evt
               always-evt)
         (lifted scheme/base 
                 bytes->string/utf-8
                 guard-evt
                 wrap-evt
                 bytes=?
                 hash-ref!)
         (lifted mzscheme 
                 make-hash-table
                 open-output-bytes
                 write
                 get-output-bytes
                 regexp-replace*)
         "../../js/js.ss"
         "../comet-ms/kill-safe-ms.ss"
         "model-fr.ss"
         "view-ajax2-fr.ss")

(provide;/contract
 make-start ;(picker? . -> . (request? . -> . response/c))]
 )

;; change this value to adjust the number of team that are allowed to be made
(define num-of-teams 3)

;; start: request -> goes to login page
(define ((make-start picker) request)
  (show-login picker))

;; show-login-page: picker String -> void
;; calls the render login with the appropiate data
(define (show-login picker)
  (let* ([login-handler 
          (lambda (request)
            (define username 
              (binding->string #"e-mail" request))
            (define password
              (binding->string #"password" request))
            (show-profile picker
                          (user-login picker username password)))]
         
         [new-profile-handler 
          (lambda (request)
            (show-new-profile picker))]
         
         [response-generator 
          (lambda (make-url)
            (render-login (make-url login-handler)
                          (make-url new-profile-handler)))])
    
    (send/suspend/dispatch response-generator)))

;; show-new-profile-page: request -> void
;; calls the render-new-profile with the needed information
(define (show-new-profile picker)
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
            
            (show-profile picker
                          (add-user! picker
                                     first-name 
                                     last-name 
                                     e-mail 
                                     password 
                                     (string->number section)
                                     (list))))]
         
         [response-generator 
          (lambda (make-url)
            (render-new-profile (make-url availability-handler)))])
    
    (send/suspend/dispatch response-generator)))

(require tests/eli-tester)
(define (quote-string s)
  (regexp-replace* "%22" s "%5C%22"))

(define (escape-string s)
  (regexp-replace* "%20" s " "))


;; show-profile-page: boolean User request -> void
;; calls the render-profile with the correct information 
(define (show-profile picker
                      user)
  (let* ([script
          (javascript
           (var xmlHttp)
           (function 
            sendUpdate (url)
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
                      (= xmlHttp 
                         (new ActiveXObject "Microsoft.XMLHTTP")))
                     (catch exn
                       (block
                        (alert 
                         "Unable to create an XMLHTTP instance"))))))))))
            ((field xmlHttp open) "POST" url #t)
            ((field xmlHttp send) 
             (field 
              ((field document getElementById)
               "status") 
              value))))]
         
         
         [requests-handler 
          (lambda (request)
            (redirect/get)
            (show-requests picker
                           user
                           (user-hash user)))]
         
         [init-requests-handler 
          (lambda (request)
            (redirect/get)              
            (show-requests picker
                           user
                           #f))]
         
         [availability-handler 
          (lambda (request)
            (redirect/get)
            (show-availability picker
                               user))]
         
         [request-status-handler 
          (lambda (info)
            (lambda (request)
              (redirect/get)
              (show-profile picker ;;XXX possible error here was passing in the view
                            user)))]
         
         [change-time-handler 
          (lambda (info)
            (lambda (request)
              (change-available-time! 
               user 
               (floor (/ (cadr info) 24)) 
               (modulo (cadr info) 24))
              (db-changed!)
              (redirect/get)
              (show-availability picker
                                 user)))]
         
         [requests-handler 
          (lambda (info)
            (lambda (request)
              (redirect/get)
              (show-requests picker
                             user
                             (user-hash user))))]
         
         [submit-request-handler 
          (lambda (info)
            (lambda (request)
              (request-partner! user (cadr info) (car info))
              (db-changed!)
              (redirect/get)
              (show-requests picker
                             user
                             #f)))]
         
         [accept-request-handler 
          (lambda (info)
            (lambda (request)
              (accept-request! user (cadr info) (caddr info))
              ;(remove-request! user (caddr info))
              ;(remove-request! (cadr info) (caddr info))
              (db-changed!)
              (redirect/get)
              (show-requests picker
                             user
                             #f)))]
         
         [response-generator
          (lambda (make-url)
            (render-profile 
             (javascript: (getXmlAvailability
                           #,(quote-string 
                              (make-url availability-handler))))
             (user-first-name user)
             (user-last-name user)
             (user-section user)
             (user-email user)
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
             script
             (lambda (info)
               (javascript: (getXmlHttpObject
                             #,(quote-string
                                (make-url 
                                 (change-time-handler info))))))
             (make-availability-table user)
             (lambda (info)
               (javascript: (getXmlRequests
                             #,(quote-string
                                (make-url 
                                 (submit-request-handler info)))
                             #,(quote-string
                                (make-url 
                                 (requests-handler info))))))
             (lambda (info)
               (javascript: (getXmlRequests
                             #,(quote-string
                                (make-url 
                                 (accept-request-handler info)))
                             #,(quote-string
                                (make-url 
                                 (requests-handler info))))))
             (make-team-table user)
             (make-pending-table user)
             (make-request-table user)
             (make-candidate-table user)))])
    
    (send/suspend/dispatch response-generator)))

;; show-request-page: boolean user request -> void
;; calls render-requests with the correct information
(define (show-requests picker
                       user
                       data-rendered)
  (when data-rendered
    (printf "waiting for ~S profile.~n" (user-first-name user))
    (wait-for-update user data-rendered))
  (printf "showing requests for ~S.~n" (user-first-name user))
  (let* ([requests-handler 
          (lambda (info)
            (lambda (request)
              (redirect/get)
              (show-requests picker
                             user
                             (user-hash user))))]
         
         [submit-request-handler 
          (lambda (info)
            (lambda (request)
              (request-partner! user (cadr info) (car info))
              (db-changed!)
              (redirect/get)
              (show-requests picker
                             user
                             #f)))]
         
         [accept-request-handler 
          (lambda (info)
            (lambda (request)
              (accept-request! user (cadr info) (caddr info))
              ;(remove-request! user (caddr info))
              ;(remove-request! (cadr info) (caddr info))
              (db-changed!)
              (redirect/get)
              (show-requests picker
                             user
                             #f)))]
         
         [response-generator 
          (lambda (make-url)
            (render-requests
             (lambda (info)
               (javascript: (getXmlRequests
                             #,(quote-string
                                (make-url 
                                 (submit-request-handler info)))
                             #,(quote-string
                                (make-url 
                                 (requests-handler info))))))
             (lambda (info)
               (javascript: (getXmlRequests
                             #,(quote-string
                                (make-url 
                                 (accept-request-handler info)))
                             #,(quote-string
                                (make-url 
                                 (requests-handler info))))))
             (make-team-table user)
             (make-pending-table user)
             (make-request-table user)
             (make-candidate-table user)))])
    
    (send/suspend/dispatch response-generator)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-team-table user)
  (lift-strict
   map 
   (lambda (user-list)
     (list (user-first-name (car user-list))
           (user-last-name (car user-list))
           (length (compare-availability user (car user-list)))
           (cadr user-list))) 
   (user-partners-b user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-pending-table user)
  (lift-strict 
   map 
   (lambda (user-list)
     (list (user-first-name (car user-list))
           (user-last-name (car user-list))
           (length (compare-availability user (car user-list)))
           (cadr user-list))) 
   (user-pendings-b user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns requests made of the user formatted for a table
(define (make-request-table user)
  (lift-strict
   map 
   (lambda (user-number-list)
     (list
      (user-first-name (car user-number-list))
      (user-last-name (car user-number-list))
      (length (compare-availability user (car user-number-list)))
      (cadr user-number-list) ;;Not sure what this should be
      (cons "Accept" user-number-list))) 
   (user-requests-b user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-candidate-table user)
  (lift-strict
   map 
   (lambda (usr)
     (list
      (user-first-name usr)
      (user-last-name usr)
      (length (compare-availability user usr))
      (build-list num-of-teams (lambda (x) (list (add1 x) usr))))) 
   (sort-availability user (user-candidates-b user))))

;; show-availability-page: boolean user request -> void
;; calls render-availability withthe correct information
(define (show-availability picker
                           user)
  (let* ([change-time-handler 
          (lambda (info)
            (lambda (request)
              (change-available-time! 
               user (floor (/ (cadr info) 24)) (modulo (cadr info) 24))
              (db-changed!)
              (redirect/get)
              (show-availability picker
                                 user)))]
         
         [response-generator 
          (lambda (make-url)
            (render-availability
             (lambda (info)
               (javascript: (getXmlHttpObject
                             #,(quote-string
                                (make-url 
                                 (change-time-handler info))))))
             (make-availability-table user)))])
    
    (send/suspend/dispatch response-generator)))

;; make-availablility-table : user -> (listof list)
;; takes a user and returns a list of list of string number
;; where the string tells if user is free or busy for the hour number
(define (make-availability-table user)
  (define times (user-availability-b user))
  (build-list 24 
              (lambda (hour) 
                (cons 
                 (format-hour hour)
                 (build-list 
                  7
                  (lambda (day)
                    (if (ormap (lambda (t)
                                 (equal? t (+ hour (* day 24))))
                               times)
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
  (define b (bindings-assq id binds))
  (if (binding:form? b)
      (bytes->string/utf-8
       (binding:form-value b))
      (error 'binding->string "Binding not received")))

;; post-data->string: request -> String
;; turns all post data into a utf-8 string
(define (post-data->string request)
  (define post (request-post-data/raw request))
  (bytes->string/utf-8
   post))