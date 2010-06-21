#lang scheme
(require "db.ss")

(provide
 user? user-first-name user-last-name user-email user-section user-requests
 add-user! picker? user-pendings user-candidates
 add-request! remove-request! add-team! remove-team! remove-user!
 user-availability set-user-availability! user-partners initialize-picker!)
(provide/contract 
 [change-available-time! (user? number? number? . -> . void?)]
 [request-partner! (user? user? number? . -> . void)]
 [accept-request! (user? user? number? . -> . void)]
 [user-login (picker? string? string? . -> . user?)]
 [sort-availability (user? (listof user?) . -> . (listof user?))]
 [compare-availability (user? user? . -> . (listof number?))])

;; time->hour: number number -> number
;; converts a time to an hour of the week
(define (time->hour day hour)
  (+ (* day 24) hour))

;; change-available-time! : user day hour -> user
;; adds or removes the time from the availability list
(define (change-available-time! user day hour)
  (set-user-availability! user
                          (local [(define time (time->hour day hour))
                                  (define timelist (user-availability user))]
                            (if (member time timelist)
                                (remove time timelist equal?)
                                (sort
                                 (cons time
                                       timelist) <)))))

;; compare-availability : user user -> (list of number)
;; Comparing users availability and returns a list with all
;;  of the times both users are available
(define (compare-availability first-user second-user)
  (local [(define first-user-times (user-availability first-user))
          (define second-user-times (user-availability second-user))]
    (filter (lambda (x) (member x first-user-times))
            second-user-times)))

;; sort-availability: user (list of user) -> (list of user)
;; Sorting by potential partners by most to least shared
;;  time available
(define (sort-availability user user-list)
  (sort user-list (lambda (x y)
                    (> (length (compare-availability user x))
                       (length (compare-availability user y))))))

;; request-partner: user user number -> void
;; Request a partner for a team
(define (request-partner! requestor requested  team-number)
  (define partners (user-partners requestor))
  (if (ormap (lambda (x) (equal? x requested)) partners)
      (error 'request-partner! "Already had this partner")
      (add-request! requestor requested team-number)))

;; accept-request: user user number -> string
;; Accept a request for a team
(define (accept-request! acceptor accepted team-number)
  (when (not (add-team! acceptor accepted team-number))
    (error 'accept-request "Request not accepted")))

;;  user-login : picker string string -> user?
;;  returns the user with the email and password is exists
;;   error if does not exsist
(define (user-login picker email pass)
  (define user (email->user picker email))
  (if (string=? pass (user-password user))
      user
      (error 'user-login "Incorrect password")))