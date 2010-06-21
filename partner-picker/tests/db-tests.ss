#lang scheme
(require (planet schematics/schemeunit:3)
         "db.ss")
(require/expose "db.ss" (user-id))

(provide db-tests)

(define (with-dummy-database test)
  (define picker #f)
  (dynamic-wind
   (lambda ()
     (set! picker (initialize-picker! ':memory:)))
   (lambda ()
     (test picker))
   (lambda ()
     (with-handlers ([exn:fail? void]) 
       (close! picker)))))

(define db-tests
  (test-suite 
   "database" 
   
   (test-case  
    "inititalize-picker!"
    (check-pred picker? (initialize-picker! ':memory:) "Could not initialize a Picker"))
   
   (test-case
    "add-user"
    (with-dummy-database
     (lambda (picker)
       (check-pred user? (add-user! picker "Dan" "Davis" "dsdavis@byu.edu" "passwd" 2 empty)
                   "Could not add user")
       (local [(define user
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 (list 1 2)))]
         (check-equal? "Jensen" (user-first-name user) "Could not get user first name")
         (check-equal? "Warnock" (user-last-name user) "Could not get user last name")
         (check-equal? "jensen@byu.edu" (user-email user) "Could not get user email")
         (check-equal? "passwd2" (user-password user) "Could not get user password")
         (check-equal? 2 (user-section user) "Could not get user section")
         (check-equal? (list 1 2) (user-availability user) "Could not get user availability")))))
   
   (test-case
    "get-user"
    (with-dummy-database
     (lambda (picker)
       (check-pred user? (email->user 
                          picker
                          (user-email (add-user! picker "Dan" "Davis" "dsdavis@byu.edu" "passwd" 2 empty)))
                   "Could not get user")
       (local [(define user
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 (list 1 2)))]
         (check-equal? (user-first-name (email->user picker (user-email user)))
                       "Jensen"
                       "Could not verify the correct user was retrieved")))))
   
   (test-case
    "remove-user"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1 
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "passdd" 1 (list 1 2)))
               (define user2
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passjw" 2 (list 2 3)))
               (define user3
                 (add-user! picker "Jay" "McCarthy" "jay@byu.edu" "passjm" 3 (list 1 3)))]
         (check-equal? (user-first-name (email->user picker (user-email user2)))
                       "Jensen"
                       "User not added to be removed")
         (check-not-exn (lambda () (remove-user! user2)) "Exception removing second user")
         (check-exn exn:fail? (lambda () (email->user picker (user-email user2))) "User was not removed")))))
   
   (test-case
    "request functionality"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1 
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "passdd" 1 (list 1 2)))
               (define user2
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passjw" 2 (list 2 3)))
               (define user3
                 (add-user! picker "Jay" "McCarthy" "jay@byu.edu" "passjm" 3 (list 1 3)))]
         (check-not-exn (lambda () (add-request! user2 user1 1)) "Exception adding a request")
         (check-not-exn (lambda () (add-request! user3 user1 2)) "Exception adding second request")
         (check-equal? (length (user-requests user1)) (length (list user2 user3)) "Didn't receive all of the requests added")
         (check-equal? (second (first (user-requests user1))) 1 "Got he wrong team number.")
         (check-not-exn (lambda () (remove-request! user2 1)) "Couldn't remove first request")
         (check-equal? (length (user-requests user1)) (length (list user3)) "Didn't remove request when user was first")
         (check-not-exn (lambda () (remove-request! user1 2)) "Couldn't remove second request")
         (check-equal? empty (user-requests user1) "Didn't remove request when user was second")))))
   
   (test-case
    "team functionality"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1 
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "passdd" 1 (list 1 2)))
               (define user2
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passjw" 2 (list 2 3)))
               (define user3
                 (add-user! picker "Jay" "McCarthy" "jay@byu.edu" "passjm" 3 (list 1 3)))
               (define user4
                 (add-user! picker "Niel" "Toronto" "niel@byu.edu" "passnt" 4 (list 1 2 3)))]
         
         (check-not-exn (lambda () (add-team! user1 user2 1)) "Exception adding first team")
         (check-not-exn (lambda () (add-team! user1 user3 2)) "Exception adding second team")
         (check-not-exn (lambda () (add-team! user1 user4 3)) "Exception adding third team")
         
         (check-not-exn (lambda () (add-team! user2 user3 3)) "Exception adding fourth team")
         (check-not-exn (lambda () (add-team! user2 user4 2)) "Exception adding fifth team")
         
         (check-not-exn (lambda () (add-team! user3 user4 1)) "Exception adding sixth team")
         
         (check-equal? (map (lambda (x) 
                              (user-id (first x)))
                            (user-partners user1))
                       (map user-id (list user2 user3 user4))
                       "Didn't get correct partners")
         (check-equal? (map (lambda (x) 
                              (user-id (first x)))
                            (user-partners user4))
                       (map user-id (list user3 user2 user1))
                       "Didn't get partners in order")
         
         (check-equal? (map (lambda (x)
                              (user-id (first x)))
                            (user-partners user2))
                       (map user-id (list user1 user4 user3))
                       "Didn't get correct user2 partners (before)")
         (check-not-exn (lambda () (remove-team! user2 user4 2)) "Exception removing team")
         (check-equal? (map (lambda (x)
                              (user-id (first x)))
                            (user-partners user2))
                       (map user-id (list user1 user3))
                       "Didn't get correct user2 partners (after)")
         
         (check-not-exn (lambda () (remove-team! user3 user1 2)) "Exception removing in reverse order")
         (check-equal? (map (lambda (x)
                              (user-id (first x)))
                            (user-partners user3))
                       (map user-id (list user4 user2))
                       "Didn't get user3 partners in order")))))
   
   (test-case
    "availability functionality"
    
    (with-dummy-database
     (lambda (picker)
       (local [(define user 
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "passdd" 1 (list 1 2)))]
         
         (check-equal? (user-availability user)
                       (list 1 2)
                       "Didn't get correct availability (before)")
         (check-not-exn (lambda () (set-user-availability! user (list 2 3 4 5))) "Exception setting availability")
         (check-equal?  (user-availability user)
                        (list 2 3 4 5)
                        "Didn't get correct availability (after)")))))))