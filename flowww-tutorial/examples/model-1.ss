#lang scheme

(require scheme/base)

(provide create-user!
         user-status
         user-status-update!
         follow-user!
         user-list
         user-follow-list)

;; Model
(define user-ht (make-hash))
(define following-ht (make-hash))

(define (create-user! name status)
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