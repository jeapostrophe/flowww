#lang frtime
(require xml)
(require web-server/servlet-env
         "../flowww-lib/flowww-lib.ss"
         "../frweb/frfx.ss"
         scheme/base)

;; Model
(define user-ht (make-hash))
(define following-ht (make-hash))

(define (create-user name status)
  (hash-set! user-ht name status))

(define (user-status name)
  (hash-ref! user-ht name (lambda ()
                            "no status")))

(define (user-status-update! username status)
  (hash-set! user-ht username status))

(define (follow-user! user usr-to-follow)
  (hash-set! following-ht user (cons usr-to-follow 
                                     (user-following user))))

(define (user-following user)
  (hash-ref! following-ht user (lambda ()
                                 empty)))

(define (user-list)
  (hash-map user-ht (lambda (k v)
                      k)))

(define (user-follow-list user)
  (map (lambda (follow-name)
         (list follow-name (user-status follow-name)))
       (user-following user)))

;; Controller
(define (show-login)
  (let* ([reg-handler
          (lambda (request)
            (define username 
              (binding->string #"user" request))
            (define status
              (binding->string #"status" request))
            (create-user username status)
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

;; View
(define (render-login login-url reg-url)
  `(html
    (head
     (title "Fritter Register Page"))
    (body ()
          (div ([id "header"])(h1 "Welcome to Fritter"))
          (div ([id "header"])(h2 "Register"))
          (form ([action ,reg-url])                
                "User: "
                (input ([type "text"] [name "user"]))
                "Status: "
                (input ([type "text"] [name "status"]))
                (input ([type "submit"] [value "Add User"])))          
          (div ([id "header"])(h2 "Login"))
          (form ([action ,login-url])                
                "User: "
                (input ([type "text"] [name "user"]))      
                (input ([type "submit"] [value "Login"]))))))

(define (render-profile name status update-url follow-url user-list following-list)
  `(html
    (head
     (title "User Feed"))
    (body ()
     (div 
      ([id "profile"])
      (h1 "Feed for " ,name)
      (form 
       ([action ,update-url])                
       "Status: "
       (input ([type "text"] [id "status"] [value ,status]))
       (input ([type "submit"] [value "Update"])))
      (form 
       ([action ,follow-url])                
       "Follow User: "
       (input ([type "text"] [id "follow"]))
       (input ([type "submit"] [value "Follow"])))
     (div
      ([id "users"])
      (h1 "User List")
      (ul
       ,@(map 
         (lambda (name)
           `(li (h4 ,name)))
         user-list))
     (div
      ([id "content"])
      (h1 "Follow feed")
      (ul
       ,@(map 
         (lambda (name-status-pair)
           `(li 
             (h3 ,(car name-status-pair))
             (h4 ,@(cdr name-status-pair))))
         following-list))))))))

;; Run Server
(define ((make-start) request)
  (show-login))

(serve/servlet (wrap-start (make-start)))