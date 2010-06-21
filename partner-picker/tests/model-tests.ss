#lang scheme
(require (planet schematics/schemeunit:3)
         "model.ss")
(require/expose "model.ss" (time->hour)) 
(require/expose "db.ss" (close!))
(require/expose "db.ss" (user-id))

(provide model-tests)

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

(define model-tests
  (test-suite 
   "model"
   
   (test-case
    "time->hour"
    (check-equal? (time->hour 0 2) 2
                  "Couldn't generate correct hours")
    (check-equal? (time->hour 4 3) 99
                  "Couldn't generate correct hours with days"))
   
   (test-case
    "change-available-time!"
    (with-dummy-database
     (lambda (picker)
       (local [(define user
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 empty))]
         (check-equal? empty (user-availability user)
                       "Could not get user availability")
         (check-not-exn (lambda () (change-available-time! user 0 10)) "Exception could not change time")
         (check-equal? (list 10) (user-availability user) 
                       "Incorrect times added")
         (check-not-exn (lambda () (change-available-time! user 0 12)) "Exception could not change another time")
         (check-not-exn (lambda () (change-available-time! user 1 10)) "Exception could not change time")
         (check-equal? (list 10 12 34) (user-availability user)
                       "Blocks merged incorrectly")))))
   
   
   (test-case
    "sort-availability"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "passwd2" 2 (list 1 5 7)))
               (define user2
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 (list 2 22 18)))
               (define user3
                 (add-user! picker "Jay" "McCarthy" "jay@byu.edu" "passwd2" 2 (list 1 5 9)))
               (define user4
                 (add-user! picker "Topher" "Fischer" "topher@thetopher.com" "topher" 42 (list 1 29 87)))]
         (check-not-exn (lambda () (sort-availability user1 (list user4 user3)))
                        "Exception Cannot Sort Candidates")
         (check-equal? (list user3 user4 user2) (sort-availability user1 (list user2 user3 user4))
                       "Candidates not sorted correctly")))))
   
   (test-case
    "request-partner!"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "passwd2" 2 (list 1 5 7)))
               (define user2
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 (list 2 22 18)))]
         (check-not-exn (lambda () (request-partner! user1 user2 1))
                        "Did not request a partner correctly")
         (check-equal? (user-id user1) (user-id (first (first (user-requests user2))))
                       "Did not get the user requested")
         (check-equal? (user-id user2) (user-id (first (first (user-pendings user1))))
                       "Did not get the user pending")))))
   
   (test-case
    "accept-request!"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "passwd2" 2 (list 1 5 7)))
               (define user2
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 (list 2 22 18)))]
         (check-not-exn (lambda () (request-partner! user1 user2 1))
                        "Did not request a partner correctly")
         (check-equal? (user-id user1) (user-id (first (first (user-requests user2))))
                       "Did not get the user requested")
         (check-equal? (user-id user2) (user-id (first (first (user-pendings user1))))
                       "Did not get the user pending")
         (check-not-exn (lambda () (accept-request! user2 user1 1))
                        "Did not accept request")))))
   
   (test-case
    "user-login"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "passwd2" 2 (list 2 14 15)))
               (define user2
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd" 2 (list 2 14 15)))]
         (check-not-exn (lambda () (user-login picker "dan@byu.edu" "passwd2")) "Exception could not login")
         (check-exn exn:fail? (lambda () (user-login picker "jensen@byu.edu" "pass")) 
                    "Exception logged in when it shouldn't have")
         (check-equal? (user-id user2) (user-id (user-login picker "jensen@byu.edu" "passwd"))
                       "Did not login correctly")))))))