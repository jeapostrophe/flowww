#lang mzscheme
(require xml)
(provide;/contract
 render-login 
 ;(#:submit-url string? 
 ;              #:new-profile-url string? 
 ;             #:error (or/c false/c string?)
 ;            . -> . xexpr/c)]
 render-new-profile 
 ;(#:submit-url string?
 ;             #:error (or/c false/c string?)
 ;            . -> . xexpr/c)]
 render-profile 
 ; (#:availability-url xexpr?
 ;                    #:requests-url xexpr?
 ;                   #:onload xexpr?
 ;                  #:error (or/c false/c string?)
 ;                 #:firstname string?
 ;                #:lastname string?
 ;               #:section number?
 ;              #:email string?
 ;             #:script xexpr?
 ;            . -> . xexpr/c)]
 render-availability
 ; (#:make-url procedure?
 ;            #:availability-data (listof list?)
 ;           #:error (or/c false/c string?)
 ;          . -> . xexpr/c)]
 render-requests
 ; (#:make-submit-request-url procedure?
 ;                           #:make-accept-request-url procedure?
 ;                          #:team-data (listof list?)
 ;                         #:pending-data (listof list?)
 ;                        #:request-data (listof list?)
 ;                       #:candidate-data (listof list?)
 ;                      #:error (or/c false/c string?)
 ;                     . -> . xexpr/c)]
 )


;; render-login : String String String -> xexpr
;; landing page to verify user information before 
;;  accessing the partner picker
(define (render-login submit-url
                      new-profile-url
                      err-msg)
  `(html
    (head
     (title "Partner Picker Home Page")
     (link ([rel "stylesheet"] [type "text/css"] [href "../partnerstyle.css"])))
    (body
     (div ([id "mainmenu"])(img ([src "../PLT-206.png"] [style "width: 5em; height: 5em;"])))
     (div ([id "header"])(h1 "Welcome to the Partner Picker"))
     
     ,@(if err-msg
           `((h2 ([class "error"]) ,err-msg))
           (list))
     (form ([action ,submit-url])
           
           "E-mail: "
           (input ([type "text"] [name "e-mail"]))           
           "Password: "
           (input ([type "password"] [name "password"]))
           (input ([type "submit"] [value "Login"]))))
    (a ([href ,new-profile-url])"Create New Profile")))

;; render-new-profile : String String -> xexpr
;; page to fill in the user information during registration
(define (render-new-profile submit-url
                            err-msg)
  `(html
    (head
     (title "New Profile Page")
     (link ([rel "stylesheet"] [type "text/css"] [href "../partnerstyle.css"])))
    (body
     (h1 "Create a New Profile")
     ,@(if err-msg
           `((h2 ([class "error"]) ,err-msg))
           (list))
     (form ([action ,submit-url])
           (p
            "E-mail: "
            (input ([type "text"] [name "e-mail"])))
           (p
            "Password: "
            (input ([type "password"] [name "password"])))
           (p
            "First Name: "
            (input ([type "text"] [name "firstname"])))
           (p
            "Last Name: "
            (input ([type "text"] [name "lastname"])))
           (p
            "Section Number: "
            (input ([type "text"] [name "section"])))
           (p
            (input ([type "submit"] [value "Add My Profile"])))))))

;; render-profile : String String String String String String number String -> xexpr
;; displays the information about the user that is logged in
(define (render-profile availability-url
                        err-msg
                        firstname
                        lastname
                        section
                        email
                        requests-url
                        onload-call
                        script)
  `(html
    (head
     (title "Profile Page")
     (link ([rel "stylesheet"] [type "text/css"] [href "../partnerstyle.css"]))
     ,script)
    (body ([onload ,onload-call])
          (div ([id "profile"] [style "position:absolute;right:75%;left:0%;vertical-align:top"])
               (h1 "Profile for " ,firstname " " ,lastname)
               ,@(if err-msg
                     `((h2 ([class "error"]) ,err-msg))
                     (list))
               (p "First Name: " ,firstname)
               (p "Last Name: " ,lastname)
               (p "E-mail: " ,email)
               (p "Section: " ,(number->string section))
               (p (a ([href ,availability-url])"Change Availability"))
               (p (a ([href ,requests-url])"Manage Requests")))
          (div ([id "ajax"] [style "position:absolute;right:0%;left:25%;vertical-align:top"])))))

;; render-availability : procedure? String (listof list) String -> xexpr
;; show the available times for the user
(define (render-availability make-url
                             availability-data
                             err-msg)
  `(div ([id "availability"])
        (h1 "Add Times You Are Available")
        ,@(if err-msg
              `((h2 ([class "error"]) ,err-msg))
              (list))
        (table ([border "1"])
               (tr (th) (th "Monday") (th "Tuesday") (th "Wednesday") (th "Thursday") (th "Friday") (th "Saturday") (th "Sunday"))
               ,@(map (lambda (list)
                        `(tr (th ,(car list)
                                 ,@(map (lambda (item)
                                          `(td (a ([href ,(make-url item)])
                                                  ,(car item))
                                               nbsp))                        
                                        (cdr list)))))
                      availability-data))))

;; render-request : procedure? procedure? String (listof list?) (listof list?) (listof list?) (listof list?) String -> xexpr
;; show the teams, requests made by you, for you, and others to request
(define (render-requests make-submit-url
                         make-accept-url
                         team-data
                         pending-data
                         request-data
                         candidate-data
                         err-msg)
  `(div ([id "requests"])
        (h1 "Requests")
        ,@(if err-msg
              `((h2 ([class "error"]) ,err-msg))
              (list))
        (div ([id "teams"]) (u (h3 "Teams"))
             (table ([border "1"])
                    (tr (th "Firstname") (th "Lastname") (th "Matching Freetime") (th "Team Number"))
                    ,@(map (lambda (list)
                             `(tr
                               (td ,(car list))
                               (td ,(cadr list))
                               (td ,(if (equal? 1 (caddr list))
                                        "1 hour"
                                        (string-append
                                         (number->string (caddr list)) " hours")))
                               (td "team " ,(number->string (cadddr list)))))
                           team-data)))
        (div ([id "response"]) (u (h3 "Response Pending"))
             (table ([border "1"])
                    (tr (th "Firstname") (th "Lastname") (th "Matching Freetime") (th "Team Number"))
                    ,@(map (lambda (list)
                             `(tr
                               (td ,(car list))
                               (td ,(cadr list))
                               (td ,(if (equal? 1 (caddr list))
                                        "1 hour"
                                        (string-append
                                         (number->string (caddr list)) " hours")))
                               (td "team " ,(number->string (cadddr list)))))
                           pending-data)))
        (div ([id "requests"]) (u (h3 "Requested You"))
             (table ([border "1"])
                    (tr (th "Firstname") (th "Lastname") (th "Matching Freetime") (th "Team Number") (th "Accept Request"))
                    ,@(map (lambda (list)
                             `(tr
                               (td ,(car list))
                               (td ,(cadr list))
                               (td ,(if (equal? 1 (caddr list))
                                        "1 hour"
                                        (string-append
                                         (number->string (caddr list)) " hours")))
                               (td "team " ,(number->string (cadddr list)))
                               (td (a ([href ,(make-accept-url (car (cddddr list)))])
                                      ,(car (car (cddddr list)))))))
                           request-data)))
        (div ([id "available"]) (u (h3 "Available"))
             (table ([border "1"])
                    (tr (th "Firstname") (th "Lastname") (th "Matching Freetime") (th "Request Partner"))
                    ,@(map (lambda (list)
                             `(tr
                               (td ,(car list))
                               (td ,(cadr list))
                               (td ,(if (equal? 1 (caddr list))
                                        "1 hour"
                                        (string-append
                                         (number->string (caddr list)) " hours")))
                               (td ,@(map (lambda (team)
                                            `(span
                                              " "
                                              ,(if (or (ormap (lambda (x)
                                                                (equal? (car list) (car x))
                                                                (equal? (cadr list) (cadr x)))
                                                              team-data)
                                                       (ormap (lambda (x)
                                                                (equal? (car team) (cadddr x)))
                                                              team-data)
                                                       (ormap (lambda (x)
                                                                (and (equal? (car team) (cadddr x))
                                                                     (equal? (car list) (car x))
                                                                     (equal? (cadr list) (cadr x))))
                                                              pending-data))
                                                   (string-append
                                                    "team"
                                                    (number->string (car team)))
                                                   `(a ([href ,(make-submit-url team)])
                                                       ,(string-append
                                                         "team"
                                                         (number->string (car team)))))))
                                          (cadddr list)))))
                           candidate-data)))))
