#lang scheme

(require (planet byu-plt/flowww:1:0))

(provide user-status-b follow-user! unfollow-user! user-status-update!
         user-follow-page-b make-user user-name get-user create-user)

(define-struct user (name))

(define status-ht (make-hash))
(define following-ht (make-hash))
(define user-ht (make-hash))

(define (get-user name)
  (hash-ref! user-ht name (lambda () #f)))

(define (create-user name)
  (hash-set! user-ht name (make-user name)))

(define (user-status-update! user msg)
  (hash-set! status-ht user msg)
  (changed (func-arg-evt user-status user)))

(define (user-status user)
  (hash-ref! status-ht user (lambda ()
                              "no status")))

(define user-status-b (func-arg-bhv user-status))

(define (follow-user! user usr-to-follow)
  (hash-set! following-ht user (cons usr-to-follow 
                                     (user-following user)))
  (changed (func-arg-evt user-following user)))

(define (unfollow-user! user usr-to-unfollow)
  (hash-set! following-ht user 
             (remove usr-to-unfollow 
                     (user-following user)
                     (lambda (usr1 usr2)
                       (equal? (get-user usr1)
                               (get-user usr2)))))
  (changed (func-arg-evt user-following user)))

(define (user-following user)
  (hash-ref! following-ht user (lambda ()
                                 empty)))

(define user-following-b (func-arg-bhv user-following))

(define (user-follow-page user)
  (map (lambda (usr)
         (list (user-name usr) (user-status-b usr)))
       (user-following user)))

(define user-follow-page-b (func-arg-bhv user-follow-page))