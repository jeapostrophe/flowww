#lang scheme
(require (planet jaymccarthy/fra:2:0)
         (planet dherman/set:4/set))

(define-struct user (id first-name last-name email password section available))

(provide relation-with-ids->user
         relation-remover
         overwrite-availability
         make-user user-id user-first-name user-last-name user?
         user-email user-password user-section user-available
         tuple->list)

(define (relation-with-ids->user r)
  (map 
   (lambda (tuple)
     (build-list (tuple-length tuple) (lambda (x) (tuple-ref tuple x))))
   (set->list (relation-tuples r))))

(define (relation-remover a-db table r)
  (for/fold ([a-db a-db])
    ([t (in-set (relation-tuples r))])
    (database-delete
     a-db table t)))

(define (tuple->list tuple)
  (build-list (tuple-length tuple) (lambda (x) (tuple-ref tuple x)))) 

(define (overwrite-availability a-db user times)
  (database-insert
   (database-delete 
    a-db
    'users
    (Tuple (user-id user) (user-first-name user) (user-last-name user)
           (user-email user) (user-password user) (user-section user) (user-available user)))
   'users
   (Tuple (user-id user) (user-first-name user) (user-last-name user)
          (user-email user) (user-password user) (user-section user) times)))