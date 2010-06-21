#lang scheme
(require xml)
(provide/contract
 [render-login 
  (#:submit-url string? 
                #:new-profile-url string? 
                #:error (or/c false/c string?)
                . -> . xexpr/c)]
 [render-new-profile 
  (#:submit-url string?
                #:error (or/c false/c string?)
                . -> . xexpr/c)]
 [render-profile 
  (#:availability-url xexpr?
                      #:requests-url xexpr?
                      #:error (or/c false/c string?)
                      #:firstname string?
                      #:lastname string?
                      #:section number?
                      #:email string?
                      #:script xexpr?
                      #:make-url procedure?
                      #:availability (listof list?)
                      #:make-submit-request-url procedure?
                      #:make-accept-request-url procedure?
                      #:team-data (listof list?)
                      #:pending-data (listof list?)
                      #:request-data (listof list?)
                      #:candidate-data (listof list?)
                      #:view string?
                      . -> . xexpr/c)]
 [render-availability
  (#:make-url procedure?
              #:availability (listof list?)
              #:error (or/c false/c string?)
              #:view string?
              . -> . xexpr/c)]
 [render-requests
  (#:make-submit-request-url procedure?
                             #:make-accept-request-url procedure?
                             #:team-data (listof list?)
                             #:pending-data (listof list?)
                             #:request-data (listof list?)
                             #:candidate-data (listof list?)
                             #:error (or/c false/c string?)
                             #:view string?
                             . -> . xexpr/c)])

;; render-login : String String String -> xexpr
;; landing page to verify user information before 
;;  accessing the partner picker
(define (render-login #:submit-url submit-url
                      #:new-profile-url new-profile-url
                      #:error err-msg)
  `(html
    (head
     (title "Partner Picker Home Page")
     (link ([rel "stylesheet"] [type "text/css"] [href "../partnerstyle.css"])))
    (body
     (div ([id "mainmenu"])(img ([src "../PLT-206.png"] [style "width: 5em; height: 5em;"])))
     (div ([id "header"])(h1 "Welcome to the Partner Picker"))
     
     ,@(if err-msg
           `((h2 ([class "error"]) ,err-msg))
           empty)
     (form ([action ,submit-url])
           
           "E-mail: "
           (input ([type "text"] [name "e-mail"]))           
           "Password: "
           (input ([type "password"] [name "password"]))
           (input ([type "submit"] [value "Login"]))))
    (a ([href ,new-profile-url])"Create New Profile")))

;; render-new-profile : String String -> xexpr
;; page to fill in the user information during registration
(define (render-new-profile #:submit-url submit-url
                            #:error err-msg)
  `(html
    (head
     (title "New Profile Page")
     (link ([rel "stylesheet"] [type "text/css"] [href "../partnerstyle.css"])))
    (body
     (h1 "Create a New Profile")
     ,@(if err-msg
           `((h2 ([class "error"]) ,err-msg))
           empty)
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
(define (render-profile #:availability-url availability-url
                        #:requests-url requests-url
                        #:error err-msg
                        #:firstname firstname
                        #:lastname lastname
                        #:section section
                        #:email email
                        #:script script
                        #:availability availability-data
                        #:make-url make-url
                        #:make-accept-request-url make-accept-url
                        #:make-submit-request-url make-submit-url
                        #:team-data team-data
                        #:pending-data pending-data
                        #:request-data request-data
                        #:candidate-data candidate-data
                        #:view view)
  `(html
    (head
     (title "Profile Page")
     (link ([rel "stylesheet"] [type "text/css"] [href "../partnerstyle.css"]))
     ,script)
    (body
     (div ([id "profile"] [style "position:absolute;right:75%;left:0%;vertical-align:top"])
          (h1 "Profile for " ,firstname " " ,lastname)
          ,@(if err-msg
                `((h2 ([class "error"]) ,err-msg))
                empty)
          (p "First Name: " ,firstname)
          (p "Last Name: " ,lastname)
          (p "E-mail: " ,email)
          (p "Section: " ,(number->string section))
          (p (a ([href ,availability-url])"Change Availability"))
          (p (a ([href ,requests-url])"Manage Requests")))
     ,(render-availability #:make-url make-url
                           #:availability availability-data
                           #:error err-msg
                           #:view view)
     ,(render-requests #:make-accept-request-url make-accept-url
                       #:make-submit-request-url make-submit-url
                       #:team-data team-data
                       #:pending-data pending-data
                       #:request-data request-data
                       #:candidate-data candidate-data
                       #:error err-msg
                       #:view view))))

;; render-availability : procedure? String (listof list) String -> xexpr
;; show the available times for the user
(define (render-availability #:make-url make-url
                             #:availability availability-data
                             #:error err-msg
                             #:view view)
  `(div ([id "availability"] [style ,(string-append "position:absolute;right:0%;left:25%;vertical-align:top;visibility:"
                                                   (if (equal? view "availability")
                                                       "visible"
                                                       "hidden"))])
        (h1 "Add Times You Are Available")
        ,@(if err-msg
              `((h2 ([class "error"]) ,err-msg))
              empty)
        (table ([border "1"])
               (tr (th) (th "Monday") (th "Tuesday") (th "Wednesday") (th "Thursday") (th "Friday") (th "Saturday") (th "Sunday"))
               ,@(map (lambda (list)
                        `(tr (th ,(first list)
                                 ,@(map (lambda (item)
                                          `(td (a ([href ,(make-url item)])
                                                  ,(first item))
                                               nbsp))                        
                                        (rest list)))))
                      availability-data))))

;; render-request : procedure? procedure? String (listof list?) (listof list?) (listof list?) (listof list?) String -> xexpr
;; show the teams, requests made by you, for you, and others to request
(define (render-requests
         #:make-accept-request-url make-accept-url
         #:make-submit-request-url make-submit-url
         #:team-data team-data
         #:pending-data pending-data
         #:request-data request-data
         #:candidate-data candidate-data
         #:error err-msg
         #:view view)
  `(div ([id "requests"] [style ,(string-append "position:absolute;right:0%;left:25%;vertical-align:top;visibility:"
                                               (if (equal? view "requests")
                                                   "visible"
                                                   "hidden"))])
        (h1 "Requests")
        ,@(if err-msg
              `((h2 ([class "error"]) ,err-msg))
              empty)
        (div ([id "teams"]) (u (h3 "Teams"))
             (table ([border "1"])
                    (tr (th "Firstname") (th "Lastname") (th "Matching Freetime") (th "Team Number"))
                    ,@(map (lambda (list)
                             `(tr
                               (td ,(first list))
                               (td ,(second list))
                               (td ,(if (equal? 1 (third list))
                                        "1 hour"
                                        (string-append
                                         (number->string (third list)) " hours")))
                               (td "team " ,(number->string (fourth list)))))
                           team-data)))
        (div ([id "response"]) (u (h3 "Response Pending"))
             (table ([border "1"])
                    (tr (th "Firstname") (th "Lastname") (th "Matching Freetime") (th "Team Number"))
                    ,@(map (lambda (list)
                             `(tr
                               (td ,(first list))
                               (td ,(second list))
                               (td ,(if (equal? 1 (third list))
                                        "1 hour"
                                        (string-append
                                         (number->string (third list)) " hours")))
                               (td "team " ,(number->string (fourth list)))))
                           pending-data)))
        (div ([id "requests"]) (u (h3 "Requested You"))
             (table ([border "1"])
                    (tr (th "Firstname") (th "Lastname") (th "Matching Freetime") (th "Team Number") (th "Accept Request"))
                    ,@(map (lambda (list)
                             `(tr
                               (td ,(first list))
                               (td ,(second list))
                               (td ,(if (equal? 1 (third list))
                                        "1 hour"
                                        (string-append
                                         (number->string (third list)) " hours")))
                               (td "team " ,(number->string (fourth list)))
                               (td (a ([href ,(make-accept-url (fifth list))])
                                      ,(first (fifth list))))))
                           request-data)))
        (div ([id "available"]) (u (h3 "Available"))
             (table ([border "1"])
                    (tr (th "Firstname") (th "Lastname") (th "Matching Freetime") (th "Request Partner"))
                    ,@(map (lambda (list)
                             `(tr
                               (td ,(first list))
                               (td ,(second list))
                               (td ,(if (equal? 1 (third list))
                                        "1 hour"
                                        (string-append
                                         (number->string (third list)) " hours")))
                               (td ,@(map (lambda (team)
                                            `(span
                                              " "
                                              ,(if (or (ormap (lambda (x)
                                                                (equal? (first list) (first x))
                                                                (equal? (second list) (second x)))
                                                              team-data)
                                                       (ormap (lambda (x)
                                                                (equal? (first team) (fourth x)))
                                                              team-data)
                                                       (ormap (lambda (x)
                                                                (and (equal? (first team) (fourth x))
                                                                     (equal? (first list) (first x))
                                                                     (equal? (second list) (second x))))
                                                              pending-data))
                                                   (string-append
                                                    "team"
                                                    (number->string (first team)))
                                                   `(a ([href ,(make-submit-url team)])
                                                       ,(string-append
                                                         "team"
                                                         (number->string (first team)))))))
                                          (fourth list)))))
                           candidate-data)))))

