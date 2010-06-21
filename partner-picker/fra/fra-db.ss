#lang frtime
;(require (prefix-in sqlite: (planet jaymccarthy/sqlite:4)))

(require (lifted texpict/mrpict colorize vl-append vr-append text
                 cc-superimpose hb-append ht-append pin-over blank
                 dc-for-text-size)
         (lifted texpict/utils filled-rectangle rectangle)
         frtime/etc frtime/list
         ;frtime/gui/fred mred
         (only frtime/frp-core do-in-manager do-in-manager-after)
         (only (planet jaymccarthy/fra:2:0) Tuple)
         (lifted:nonstrict (planet jaymccarthy/fra:2:0)
                           make-prop:op
                           database-insert
                           database-delete
                           query-selection
                           query-relation
                           query-projection
                           execute-query
                           relation-tuples
                           tuple-ref
                           tuple-length
                           call-with-database)
         (lifted:nonstrict (planet dherman/set:4/set)
                           set->list)
         scheme/port)


(define-struct user (id first-name last-name email password section available))

(define (relation-with-ids->user r)
  (map 
   (lambda (tuple)
     (build-list (tuple-length tuple) (lambda (x) (tuple-ref tuple x))))
   (set->list (relation-tuples r))))

(define (relation-remover a-db table r)
  (foldl (lambda (t a-db)
           (database-delete a-db table t))
         a-db
         (set->list (relation-tuples r))))

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

; A user is a (make-user string:first-name 
;                        string:last-name 
;                        string:email 
;                        string:password
;                        number:section)

; A picker-db is a (make-picker db)
; where db is an sqlite database handle
#;(define-struct picker (db))

(provide;/contract
 ;initialize-picker! ;((or/c path-string? (symbols ':memory: ':temp:)) . -> . picker?)]
 ;picker? ;(any/c . -> . boolean?)]
 ;close! ;(picker? . -> . void)]
 user? ;(any/c . -> . boolean?)]
 user-first-name ;(user? . -> . string?)]
 user-last-name ;(user? . -> . string?)]
 user-email ;(user? . -> . string?)]
 user-password ;(user? . -> . string?)]
 user-section ;(user? . -> . number?)]
 add-user! ;(picker? string? string? string? string? number? (listof number?) . -> . user?)]
 email->user ;(picker? string? . -> . user?)]
 user-requests ;(user? . -> . (listof list?))]
 user-pendings ;(user? . -> . (listof list?))]
 user-candidates ;(user? . -> . (listof user?))]
 add-team! ;(user? user? number? . -> . void)]
 add-request! ;(user? user? number? . -> . void)]
 remove-request! ;(user? number? . -> . void)]
 remove-team! ;(user? user? number? . -> . void)]
 remove-user! ;(user? . -> . void)]
 user-availability ;(user? . -> . (listof number?))]
 set-user-availability! ;(user? (listof number?) . -> . void)]
 user-partners ;(user? . -> . (listof list?))]
 id->user ;(picker? number? . -> . user?)]
 ;total-changes ;(user? . -> . number?)])
 )

; initialize-picker-db! : path? -> picker?
; Sets up a picker-db database (if it doesn't exist)

(define next-id 0)

(define (get-next-id)
  (begin0
    next-id
    (set! next-id (add1 next-id))))

#;(define a-db
    (box
     (Database
      [users
       [id fname lname email password section available]
       (1 "f1" "l1" "1" "1" 1 (list 1 2 3 4 5))
       (2 "f2" "l2" "2" "2" 2 (list 2 3 4 5 6))]
      [teams
       [id1 id2 number]
       (1 2 1)
       (2 1 2)]
      [requests
       [id1 id2 number]
       (1 2 3)
       (2 1 4)])))

;;  add-user: firstname lastname email password section available -> user?
;;  adds the user info to the db, and returns a user?
(define (add-user! a-db
                   firstname
                   lastname
                   email
                   password
                   section
                   available)
  (define times (with-output-to-string (lambda () (printf "~S" available))))
  (database-insert 
   a-db
   'users
   (Tuple (get-next-id) firstname lastname email password section times))
  (email->user a-db email))

;;  get-user: picker email -> user?
;;  returns the user with the matching email
(define (email->user a-db email-addy)
  (define id (tuple-ref
    (car
     (set->list
      (relation-tuples
       (call-with-database 
        a-db
        (lambda ()
          (execute-query                   
           (query-projection
            '(id)
            (query-selection
             (make-prop:op (lambda (x)
                             (value-now (equal? email-addy x)))
                           '(email))
             (query-relation 'users)))))))))
    0))
    (printf "id ~S~n" (value-now id))
  (id->user 
   a-db
   id))

;;  user-requests: user -> (listof user?)
;;  returns the list containing all users that have requested the
;;   user as a partner
(define (user-requests a-db user)
  (map 
   (lambda (x) 
     (list (id->user a-db (list-ref x 0)) (list-ref x 1)))
   (relation-with-ids->user 
    (call-with-database
     a-db
     (lambda ()
       (execute-query                   
        (query-projection
         '(id1 number)
         (query-selection
          (make-prop:op (lambda (x) 
                          (value-now (equal? (user-id user) x)))
                        '(id2))
          (query-relation 'requests)))))))))

;;  user-pendings: user -> (listof user?)
;;  returns the list of all users that the user has requested
(define (user-pendings a-db user)
  (map 
   (lambda (x) 
     (list (id->user a-db (list-ref x 0)) (list-ref x 1)))
   (relation-with-ids->user 
    (call-with-database 
     a-db
     (lambda ()
       (execute-query                   
        (query-projection
         '(id2 number)
         (query-selection
          (make-prop:op (lambda (x) 
                          (value-now (equal? (user-id user) x)))
                        '(id1))
          (query-relation 'requests)))))))))

;;  user-candidates: user -> (listof user?)
;;  returns all of the users other than user given
(define (user-candidates a-db user)
  (map 
   (lambda (x) 
     (id->user a-db (list-ref x 0)))
   (relation-with-ids->user 
    (call-with-database
     a-db
     (lambda ()
       (execute-query
        (query-projection
         '(id)
         (query-selection
          (make-prop:op (lambda (x) 
                          (value-now (not (equal? (user-id user) x))))
                        '(id))
          (query-relation 'users)))))))))

;; add-team: fra user user number -> fra
;; adds a team to the db
(define (add-team! a-db
                   user1
                   user2
                   team)
  (database-insert 
   a-db
   'teams
   (Tuple (user-id user1) (user-id user2) team)))

;; add-request: fra user user team-> fra
;; adds a request to the db
(define (add-request! a-db
                      user1
                      user2
                      team)
  
  (database-insert 
   a-db
   'requests
   (Tuple (user-id user1) (user-id user2) team)))

;;  remove-request: fra user number -> fra
;;  removes all requests containing the user for that team 
(define (remove-request! a-db
                         user
                         team-num)
  (relation-remover
   a-db
   'requests
   (call-with-database a-db
                       (lambda ()
                         (execute-query
                          (query-selection
                           (make-prop:op (lambda (x1 x2 n)
                                           (value-now (or (and (equal? x1 (user-id user)) (equal? n team-num))
                                                          (and (equal? x2 (user-id user)) (equal? n team-num)))))
                                         '(id1 id2 number))
                           (query-relation 'requests)))))))

;;  remove-team: fra user user number-> fra
;;  removes a the team from the db
(define (remove-team! a-db
                      user1
                      user2
                      team)
  (relation-remover 
   a-db
   'teams
   (call-with-database a-db
                       (lambda ()
                         (execute-query
                          (query-selection
                           (make-prop:op (lambda (x1 x2 n)
                                           (value-now (or (and (equal? x1 (user-id user1)) (and (equal? x2 (user-id user2)) (equal? n team)))
                                                          (and (equal? x1 (user-id user2)) (and (equal? x2 (user-id user1)) (equal? n team))))))
                                         '(id1 id2 number))
                           (query-relation 'teams)))))))

;;  remove-user: fra user -> fra
;;  removes user from the db
(define (remove-user! a-db user)  
  (database-delete 
   a-db
   'users
   (Tuple (user-id user) (user-first-name user) (user-last-name user)
          (user-email user) (user-password user) (user-section user) (user-available user))))

;;  user-availability: fra user -> (listof number?)
;;  returns list of numbers representing the hours of the week
;;   the user is available
(define (user-availability a-db user)
  (car
   (tuple->list                           
    (car
     (set->list
      (relation-tuples
       (call-with-database 
        a-db
        (lambda ()
          (execute-query                   
           (query-projection
            '(available)
            (query-selection
             (make-prop:op (lambda (x)
                             (value-now (equal? x (user-id user))))
                           '(id))
             (query-relation 'users))))))))))))

;;  set-user-availability!: fra user (listof number) -> fra
;;  sets the availability to the list of numbers
(define (set-user-availability! a-db
                                user
                                availability)
  (define times (with-output-to-string (lambda () (printf "~S" availability))))
  (overwrite-availability a-db user times))

;; user-partners: fra user -> (listof list)
;; returns (listof (list user? number?)
;;  where users is the partner for the team represented by number
(define (user-partners a-db user)
  (map
   (lambda (x)
     (if (equal? (tuple-ref x 0) (user-id user))
         (list (id->user a-db (tuple-ref x 1)) (tuple-ref x 2))
         (list (id->user a-db (tuple-ref x 0)) (tuple-ref x 2))))
   (set->list
    (relation-tuples
     (call-with-database 
      a-db
      (lambda ()
        (execute-query                   
         (query-projection
          '(id1 id2 number)
          (query-selection
           (make-prop:op (lambda (x1 x2)
                           (value-now (or (equal? (user-id user) x1)
                                          (equal? (user-id user) x2))))
                         '(id1 id2))
           (query-relation 'teams))))))))))

;; id->user : fra id -> user
;; returns the user with the id
(define (id->user a-db usr-id)
  (printf "id->user ~S~n" (value-now usr-id))
  (let ([t (relation-tuples
            (call-with-database
             a-db
             (lambda ()
               (execute-query
                (query-projection
                 '(fname lname email password section available)
                 (query-selection
                  (make-prop:op (lambda (x) 
                                  (equal? (value-now usr-id) x))
                                '(id))
                  (query-relation 'users)))))))])
        (let ([tup (tuple->list (car (set->list t)))])
          (apply make-user usr-id tup))))
