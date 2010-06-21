#lang scheme
(require (planet schematics/schemeunit:3)
         "model.ss")
(require/expose "control.ss"(format-hour))
(require/expose "control.ss"(make-availability-table))
(require/expose "control.ss"(make-pending-table))
(require/expose "control.ss"(make-request-table))
(require/expose "control.ss"(make-candidate-table))
(require/expose "control.ss"(make-team-table))
(require/expose "db.ss" (close!))

(provide control-tests)

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

(define control-tests
  (test-suite 
   "controller"
   (test-case
    "format-hour"
    (check-equal? (format-hour 10) "10:00 am"
                  "Did not format the correct time below 12")
    (check-equal? (format-hour 18) "6:00 pm"
                  "Did not format the correct time above 12"))
   
   (test-case
    "make-availability-table"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 empty))
               (define user2
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "pass" 2 (list 1 2 23)))]
         (check-equal? (make-availability-table user1)(build-list 24
                                                                  (lambda (hour)
                                                                    (cons(format-hour hour)
                                                                         (build-list 7
                                                                                     (lambda (day)
                                                                                       (list "Busy"(+ hour (* day 24))))))))
                       "Did not build the availability table correct")
         (check-equal? (make-availability-table user2)(build-list 24
                                                                  (lambda (hour)
                                                                    (cons(format-hour hour)
                                                                         (build-list 7
                                                                                     (lambda (day)
                                                                                       (if (equal? (+ hour (* day 24)) 1)
                                                                                           (list "Free" 1)
                                                                                           (if (equal? (+ hour (* day 24)) 2)
                                                                                               (list "Free" 2)
                                                                                               (if (equal? (+ hour (* day 24)) 23)
                                                                                                   (list "Free" 23)
                                                                                                   (list "Busy"(+ hour (* day 24)))))))))))
                       "Did not build the availability table correct")))))
   (test-case
    "make-team-table"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 empty))
               (define user2
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "pass" 2 (list 1 2 23)))]
         (add-team! user1 user2 1)
         (check-equal? (make-team-table user1)(list (list "Dan" "Davis" 0 1))
                       "Did not make the correct team table")
         (check-equal? (make-team-table user2)(list (list "Jensen" "Warnock" 0 1))
                       "Did not make the correct team table")))))
   
   ;;binding->string
   ;;make-pending-table
   (test-case
    "make-pending-table"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 empty))
               (define user2
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "pass" 2 (list 1 2 23)))]
         (request-partner! user1 user2 1)
         (check-equal? (make-pending-table user1)(list (list "Dan" "Davis" 0 1))
                       "Did not make the correct pending table")))))
   ;;make-request-table
   (test-case
    "make-request-table"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 empty))
               (define user2
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "pass" 2 (list 1 2 23)))]
         (request-partner! user1 user2 1)
         (check-eq? (make-request-table user2)(list (list "Jensen" "Warnock" 0 1 (list "Accept" user1 1)))
                       "Did not make the correct request table")))))
   ;;make-candidate-table
   (test-case
    "make-candidate-table"
    (with-dummy-database
     (lambda (picker)
       (local [(define user1
                 (add-user! picker "Jensen" "Warnock" "jensen@byu.edu" "passwd2" 2 empty))
               (define user2
                 (add-user! picker "Dan" "Davis" "dan@byu.edu" "pass" 2 (list 1 2 23)))
               (define user3
                 (add-user! picker "Jay" "McCarthy" "jay@byu.edu" "pass" 2 (list 3 5 26)))]
         (add-team! user1 user2 1)
         (check-equal? (make-candidate-table user1)(list (list "Dan" "Davis" 0 (list (list "1" user1) (list "2" user1)
                                                               (list "3" user1)))
                                                         (list "Jay" "McCarthy" 0 (list (list "1" user1) (list "2" user1)
                                                               (list "3" user1))))
                       "Did not make the correct candidate table")))))
         ))