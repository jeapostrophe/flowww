#lang frtime
(require xml)
(provide;/contract
 render-login
 render-profile)

(define (render-login login-url)
  `(html
    (head
     (title "Fwitter Home Page")
     (link ([rel "stylesheet"] [type "text/css"] [href "/partnerstyle.css"])))
    (body ()
          (div ([id "mainmenu"])(img ([src "/PLT-206.png"] [style "width: 5em; height: 5em;"])))         
          (div ([id "header"])(h1 "Welcome to Fwitter"))
          (form ([action ,login-url])                
                "User: "
                (input ([type "text"] [name "user"]))      
                (input ([type "submit"] [value "Login"]))))))

;; render-profile : String String String String String String number String -> xexpr
;; displays the information about the user that is logged in
(define (render-profile name
                        status
                        update-status-url
                        following-data
                        script)
  (list 
   'html
   `(head
     (title "User Feed")
     (link 
      ([rel "stylesheet"] [type "text/css"] [href "/partnerstyle.css"]))
     ,script)
   (list 
    'body
    (list)
    `(div 
      ([id "profile"] 
       [style "position:absolute;right:75%;left:0%;vertical-align:top"])
      (h1 "Feed for " ,name)
      (form 
       ([action ,update-status-url])                
       "Change Status: "
       (input 
        ([type "text"] [id "status"] [value ,status]))      
       (input 
        ([type "submit"] [value "Change"]))))
    (list 
     'div
     `([id "ajax"]
       [style "position:absolute;right:0%;left:25%;vertical-align:top"])
     `(h1 "Follow feed")
     (append (list 'ul)
             (map 
              (lambda (name-status-pair)
                `(li 
                  (h3 ,(car name-status-pair))
                  (h4 ,@(cdr name-status-pair))))
              following-data)
             (list ""))))))