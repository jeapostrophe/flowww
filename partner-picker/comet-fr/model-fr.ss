#lang frtime
(require "../original/db.ss"
         (lifted scheme/base
                 make-hash
                 hash-ref!))

(provide
 user? user-first-name user-last-name user-email user-section
 user-requests-b add-user! picker? user-pendings-b user-candidates-b
 add-request! remove-request! add-team! remove-team! remove-user!
 user-availability-b set-user-availability! user-partners-b
 initialize-picker!)
(provide;/contract 
 change-available-time! ;(user? number? number? . -> . void?)]
 request-partner! ;(user? user? number? . -> . void)]
 accept-request! ;(user? user? number? . -> . void)]
 user-login ;(picker? string? string? . -> . user?)]
 sort-availability ;(user? (listof user?) . -> . (listof user?))]
 compare-availability ;(user? user? . -> . (listof number?))]
 )

(define unique-ht (make-hash))

(define (unique-value n init-thunk)
  (hash-ref! unique-ht n init-thunk))

(define (user-f-evt f user)
  (unique-value (cons f (user-id user))
                (lambda () (event-receiver))))

(define ((user-f-bhv f) user)
  (hold (map-e (lambda (v) (f user))
               (user-f-evt f user))
        (f user)))

(define user-availability-b (user-f-bhv user-availability))

(define user-requests-b (user-f-bhv user-requests))

(define user-pendings-b (user-f-bhv user-pendings))

(define user-partners-b (user-f-bhv user-partners))

(define user-candidates-b (user-f-bhv user-candidates))

;; time->hour: number number -> number
;; converts a time to an hour of the week
(define (time->hour day hour)
  (+ (* day 24) hour))

;; change-available-time! : user day hour -> user
;; adds or removes the time from the availability list
(define (change-available-time! user day hour)
  (begin0 
    (set-user-availability! 
     user
     (let ([time (time->hour day hour)]
           [timelist (user-availability user)])
       (if (memf (lambda (t)
                   (equal? t time))
                 timelist)
           (remove time timelist equal?)
           (sort
            (cons time
                  timelist) <))))    
    (send-event (user-f-evt user-availability user) 'changed)))

;; compare-availability : user user -> (list of number)
;; Comparing users availability and returns a list with all
;;  of the times both users are available
(define (compare-availability first-user second-user)
  (let ([first-user-times (user-availability-b first-user)]
        [second-user-times (user-availability-b second-user)])
    (lift-strict 
     filter 
     (lambda (x) (memf (lambda (t)
                         (equal? t x))
                       first-user-times))
     second-user-times)))

;; sort-availability: user (list of user) -> (list of user)
;; Sorting by potential partners by most to least shared
;;  time available
(define (sort-availability user user-list)
  (lift-strict 
   sort 
   (raise-reactivity user-list)
   (lambda (x y)
     (> (length (compare-availability user x))
        (length (compare-availability user y))))))

;; request-partner: user user number -> void
;; Request a partner for a team
(define (request-partner! requestor requested  team-number)
  (define partners (user-partners requestor))
    (if (ormap (lambda (x) (equal? x requested)) partners)
        (error 'request-partner! "Already had this partner")
        (begin
          (add-request! requestor requested team-number)  
          (send-event 
           (user-f-evt user-requests requested) 
           'changed)
          (send-event 
           (user-f-evt user-pendings requestor)
           'changed)          
          (send-event 
           (user-f-evt user-candidates requested)
           'changed)          
          (send-event
           (user-f-evt user-candidates requestor)
           'changed))))

;; accept-request: user user number -> string
;; Accept a request for a team
(define (accept-request! acceptor accepted team-number)
    (if (add-team! acceptor accepted team-number)
        (begin (remove-request! acceptor team-number)
               (remove-request! accepted team-number)
               (send-event (user-f-evt user-partners acceptor) 'changed)
               (send-event (user-f-evt user-partners accepted) 'changed)
               (send-event (user-f-evt user-candidates acceptor) 'changed)
               (send-event (user-f-evt user-candidates accepted) 'changed)
               (send-event (user-f-evt user-requests acceptor) 'changed)
               (send-event (user-f-evt user-pendings accepted) 'changed))
        (error 'accept-request "Request not accepted")))

;;  user-login : picker string string -> user?
;;  returns the user with the email and password is exists
;;   error if does not exsist
(define (user-login picker email pass)
  (define user (email->user picker email))
  (if (string=? pass (user-password user))
      user
      (error 'user-login "Incorrect password")))