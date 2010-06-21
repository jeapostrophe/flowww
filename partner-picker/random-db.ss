#lang scheme
(require mzlib/etc
         (planet schematics/schemeunit:3)
         "original/model.ss")
(require/expose "original/db.ss" (id->user))

; Help
(define (make-random i)
  (lambda () (add1 (random i))))

; DB Setup
(define USERS 5 #;20)
(define SECTIONS 2)
(define TEAMS 3)
(define REQUESTS 5 #;30)
(define MADE-TEAMS 5)

(define random-picker (initialize-picker! ':memory:))

(define random-user (make-random USERS))
(define random-section (make-random SECTIONS))
(define random-team (make-random TEAMS))

(for-each (lambda (i)
            (add-user! random-picker 
                       (format "F~a" i)
                       (format "L~a" i)
                       (format "~a" i)
                       (format "~a" i)
                       (random-section)
                       (filter (lambda _ (zero? (random 2)))
                               (build-list (* 7 24) (lambda (i) i)))))
          (build-list USERS add1))

(for-each (lambda (i j n)
            (add-request! (id->user random-picker i) (id->user random-picker j) n))
          (build-list REQUESTS (lambda _ (random-user)))
          (build-list REQUESTS (lambda _ (random-user)))
          (build-list REQUESTS (lambda _ (random-team))))

(for-each (lambda (i j n)
            (add-team! (id->user random-picker i) (id->user random-picker j) n))
          (build-list MADE-TEAMS (lambda _ (random-user)))
          (build-list MADE-TEAMS (lambda _ (random-user)))
          (build-list MADE-TEAMS (lambda _ (random-team))))

(provide random-picker)