#lang scheme
(require (prefix-in sqlite: (planet jaymccarthy/sqlite:4)))

; A user is a (make-user string:first-name 
;                        string:last-name 
;                        string:email 
;                        string:password
;                        number:section)
(define-struct user (picker id first-name last-name email password section))

; A picker-db is a (make-picker db)
; where db is an sqlite database handle
(define-struct picker (db))

(provide make-user)

(provide/contract
 [initialize-picker! ((or/c path-string? (symbols ':memory: ':temp:)) . -> . picker?)]
 [picker? (any/c . -> . boolean?)]
 [close! (picker? . -> . void)]
 [user? (any/c . -> . boolean?)]
 [user-id (user? . -> . number?)]
 [user-first-name (user? . -> . string?)]
 [user-last-name (user? . -> . string?)]
 [user-email (user? . -> . string?)]
 [user-password (user? . -> . string?)]
 [user-section (user? . -> . number?)]
 [add-user! (picker? string? string? string? string? number? (listof number?) . -> . user?)]
 [email->user (picker? string? . -> . user?)]
 [user-requests (user? . -> . (listof list?))]
 [user-pendings (user? . -> . (listof list?))]
 [user-candidates (user? . -> . (listof user?))]
 [add-team! (user? user? number? . -> . void)]
 [add-request! (user? user? number? . -> . void)]
 [remove-request! (user? number? . -> . void)]
 [remove-team! (user? user? number? . -> . void)]
 [remove-user! (user? . -> . void)]
 [user-availability (user? . -> . (listof number?))]
 [set-user-availability! (user? (listof number?) . -> . void)]
 [user-partners (user? . -> . (listof list?))]
 [id->user (picker? number? . -> . user?)]
 [total-changes (user? . -> . number?)])

; initialize-picker-db! : path? -> picker?
; Sets up a picker-db database (if it doesn't exist)
(define (initialize-picker! path)
  (define db (sqlite:open path))
  (with-handlers ([exn:fail? void])
    (sqlite:exec/ignore 
     db "CREATE TABLE users (id INTEGER PRIMARY KEY, fname TEXT, lname TEXT, email TEXT, password TEXT, section INTEGER, available TEXT)")
    (sqlite:exec/ignore 
     db "CREATE TABLE teams (teamid INTEGER PRIMARY KEY, id1 INTEGER, id2 INTEGER, number INTEGER)")
    (sqlite:exec/ignore 
     db "CREATE TABLE requests (id1 INTEGER, id2 INTEGER, number INTEGER)"))
  (make-picker db))

; total-changes : user -> number
; returns the number of changes that have been on the database
(define (total-changes user)
  (sqlite:total-changes-count 
     (picker-db (user-picker user))))

; prepare/load/step : db format . args -> (vectorof sqlite-datum/c)
; executes all of the needed methods for the sqlite query with one result
(define (prepare/load/step db fmt . args)
  (define stmt (sqlite:prepare db fmt))
  (apply sqlite:load-params stmt args)
  (begin0 (sqlite:step stmt)
          (sqlite:finalize stmt)))

; prepare/load/step : db format . args -> (listof (vectorof sqlite-datum/c))
; executes all of the needed methods for the sqlite query with multiple results
(define (prepare/load/step* db fmt . args)
  (define stmt (sqlite:prepare db fmt))
  (apply sqlite:load-params stmt args)
  (begin0 (sqlite:step* stmt)
          (sqlite:finalize stmt)))

;;  add-user: firstname lastname email password section available -> user?
;;  adds the user info to the db, and returns an user?
(define (add-user! the-picker 
                   firstname
                   lastname
                   email
                   password
                   section
                   available)
  (define times (with-output-to-string (lambda () (write available))))
  (prepare/load/step 
   (picker-db the-picker)
   "INSERT INTO users (fname, lname, email, password, section, available) VALUES (?, ?, ?, ?, ?, ?)"
   firstname lastname email password section times)
  (email->user the-picker email))

;;  get-user: picker email -> user?
;;  returns the user with the matching email
(define (email->user the-picker email)
  (define reference
    (prepare/load/step
     (picker-db the-picker)
     "SELECT id FROM users WHERE email = ?"
     email))
  (if reference
      (id->user the-picker (vector-ref reference 0))
      (error 'email->user "No user: ~e" email)))

;;  user-requests: user -> (listof user?)
;;  returns the list containing all users that have requested the
;;   user as a partner
(define (user-requests user)
  (define the-picker
    (user-picker user))
  (define search
    (prepare/load/step*
     (picker-db the-picker)
     "SELECT id1, number FROM requests WHERE id2 = ? ORDER BY number"
     (user-id user)))
  (map (lambda (x) (list (id->user the-picker (vector-ref x 0)) (vector-ref x 1)))
       search))


;;  user-pendings: user -> (listof user?)
;;  returns the list of all users that the user has requested
(define (user-pendings user)
  (define the-picker
    (user-picker user))
  (define search
    (prepare/load/step*
     (picker-db the-picker)
     "SELECT id2, number FROM requests WHERE id1 = ? ORDER BY number"
     (user-id user)))
  (map (lambda (x) (list (id->user the-picker (vector-ref x 0)) (vector-ref x 1)))
       search))

;;  user-candidates: user -> (listof user?)
;;  returns all of the users other than user given
(define (user-candidates user)
  (define the-picker
    (user-picker user))
  (define search
    (prepare/load/step*
     (picker-db the-picker)
     "SELECT id FROM users WHERE id != ?"
     (user-id user)))
  (map (lambda (x) (id->user the-picker (vector-ref x 0)))
       search))

;; add-team: user user number -> void
;; adds a team to the db
(define (add-team! user1
                   user2
                   team)
  (prepare/load/step 
   (picker-db (user-picker user1))
   "INSERT INTO teams (id1, id2, number) VALUES (?, ?, ?)"
   (user-id user1) (user-id user2) team)
  (void))

;; add-request: user user team-> void
;; adds a request to the db
(define (add-request! user1
                      user2
                      team)
  (prepare/load/step 
   (picker-db (user-picker user1))
   "INSERT INTO requests (id1, id2, number) VALUES (?, ?, ?)"
   (user-id user1) (user-id user2) team)
  (void))

;;  remove-request: user number -> void
;;  removes all requests containing the user for that team 
(define (remove-request! user team-num)
  (prepare/load/step
   (picker-db (user-picker user))
   "DELETE FROM requests WHERE id1 = ? AND number = ? OR id2 = ? AND number = ?"
   (user-id user) team-num (user-id user) team-num)
  (void))

;;  remove-team: user user number-> void
;;  removes a the team from the db
(define (remove-team! user1
                      user2
                      team)
  (prepare/load/step
   (picker-db (user-picker user1))
   "DELETE FROM teams WHERE id1 = ? AND id2 = ? AND number = ?"
   (user-id user1) (user-id user2)
   team)
  (prepare/load/step
   (picker-db (user-picker user1))
   "DELETE FROM teams WHERE id1 = ? AND id2 = ? AND number = ?"
   (user-id user2) (user-id user1)
   team)
  (void))

;;  remove-user: user -> void
;;  removes user from the db
(define (remove-user! user)
  (prepare/load/step
   (picker-db (user-picker user))
   "DELETE FROM users WHERE id = ?"
   (user-id user))
  (void))

;;  user-availability: user -> (listof number?)
;;  returns list of numbers representing the hours of the week
;;   the user is available
(define (user-availability user)  
  (define result
    (prepare/load/step
     (picker-db (user-picker user))
     "SELECT available FROM users WHERE id = ?"
     (user-id user)))
  (if result
      (with-input-from-string (vector-ref result 0) read)
      (error 'user-availability "No availability for user")))

;;  set-user-availability!: user (listof number) -> void
;;  sets the availability to the list of numbers
(define (set-user-availability! user
                                available)
  (define times (with-output-to-string (lambda () (write available))))
  (prepare/load/step 
   (picker-db (user-picker user))
   "UPDATE users SET available = ? WHERE id = ?"
   times (user-id user))
  (void))

;; user-partners: user -> (listof list)
;; returns (listof (list user? number?)
;;  where users is the partner for the team represented by number
(define (user-partners user)
  (define search
    (prepare/load/step*
     (picker-db (user-picker user))
     "SELECT id1, id2, number FROM teams WHERE id1 = ? OR id2 = ? ORDER BY number"
     (user-id user) (user-id user)))
  (if search
      (map (lambda (x)
             (if (equal? (vector-ref x 0) (user-id user))
                 (list (id->user (user-picker user) (vector-ref x 1)) (vector-ref x 2))
                 (list (id->user (user-picker user) (vector-ref x 0)) (vector-ref x 2))))
           search)
      (error 'user-partners "Cannot lookup partners")))

;; id->user : picker id -> user
;; returns the user with the id
(define (id->user the-picker
                     id)
  (define reference
    (prepare/load/step
     (picker-db the-picker)
     "SELECT fname,lname,email,password,section FROM users WHERE id = ?"
     id))
  (if reference
      (apply make-user the-picker id (vector->list reference))
      (error 'id->user "Unknown user: ~e" id)))

;; close! : picker -> void
;; close the db
(define (close! p)
  (sqlite:close (picker-db p)))
