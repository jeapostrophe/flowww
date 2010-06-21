#lang frtime
(require scheme/base
         (planet byu-plt/flowww:1:0))

(provide create-user!
         user-status-b
         user-status-update!
         follow-user!
         user-list-b
         user-follow-list-b)

;; Model
(define user-ht (make-hash))
(define following-ht (make-hash))

(define (create-user! name status)
  (hash-set! user-ht name status)
  (changed (func-arg-evt user-list)))

(define (user-status name)
  (hash-ref! user-ht name (lambda ()
                            "no status")))
(define user-status-b (func-arg-bhv user-status))

(define (user-status-update! username status)
  (hash-set! user-ht username status)
  (changed (func-arg-evt user-status username)))

(define (follow-user! user usr-to-follow)
  (hash-set! following-ht user (cons usr-to-follow 
                                     (user-following user)))
  (changed (func-arg-evt user-follow-list user)))

(define (user-following user)
  (hash-ref! following-ht user (lambda ()
                                 empty)))

(define (user-list)
  (hash-map user-ht (lambda (k v)
                      k)))
(define user-list-b (func-arg-bhv user-list))

(define (user-follow-list user)
  (map (lambda (follow-name)
         (list follow-name (user-status-b follow-name)))
       (user-following user)))

(define user-follow-list-b (func-arg-bhv user-follow-list))