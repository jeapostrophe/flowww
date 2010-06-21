#lang frtime
(provide render-login
         render-profile)

;; View
(define (render-login login-url 
                      reg-url)
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

(define (render-profile name status 
                        update-url 
                        follow-url 
                        user-list 
                        following-list 
                        script)
  `(html
    (head
     (title "User Feed")
     ,script)
    
    (body ()
          (div ([id "profile"])
               (h1 "Feed for " ,name)
               
               (form ([action ,update-url])                
                     "Status: "
                     (input ([type "text"] [id "status"] [value ,(value-now status)]))
                     (input ([type "submit"] [value "Update"])))
               
               (form ([action ,follow-url])                
                     "Follow User: "
                     (input ([type "text"] [id "follow"]))
                     (input ([type "submit"] [value "Follow"])))
               
               (div ([id "users"])
                    (h1 "User List")
                    
                    (ul
                     ,@(map 
                        (lambda (name)
                          `(li (h4 ,name)))
                        user-list))
                    
                    (div ([id "content"])
                         (h1 "Follow feed")
                         
                         (ul
                          ,@(map 
                             (lambda (name-status-pair)
                               `(li 
                                 (h3 ,(car name-status-pair))
                                 (h4 ,@(cdr name-status-pair))))
                             following-list))))))))