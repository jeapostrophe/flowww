#lang frtime
(require (lifted texpict/mrpict dc-for-text-size)
         frtime/gui/fred mred frtime/etc frtime/list
         (only frtime/frp-core do-in-manager do-in-manager-after)
         (planet jaymccarthy/fra:2:0)
         "random-db.ss"
         "fra-model.ss")

(define sizing-dc
  (new bitmap-dc% [bitmap (make-object bitmap% 64 64)]))

(dc-for-text-size sizing-dc)
(do-in-manager
 (dc-for-text-size sizing-dc))

(define size (new-cell 20))

(do-in-manager-after (void))

(define frame (new ft-frame% [label "Partner Picker"] [shown #t]
                   [min-width (* size 50)] [min-height (* size 50)]))

(define a-font (make-object font% 16 'roman 'normal 'bold))

(define hl-pane (new horizontal-pane% 
                     [parent frame] 
                     [alignment '(left top)]))

(define vt-profile-pane (new vertical-pane% 
                             [parent hl-pane] 
                             [alignment '(left top)]))         

(define (make-team-table a-db user)
  (printf "Team-Table-User: ~S~n" (value-now user))
  (map (lambda (user-list)
         (list (user-first-name (first user-list))
               (user-last-name (first user-list))
               (length (compare-availability a-db user (first user-list)))
               (second user-list)))
       (user-partners a-db user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-pending-table a-db user)
  (printf "Pending-Table-User: ~S~n" (value-now user))
  (map (lambda (user-list)
         (list (user-first-name (first user-list))
               (user-last-name (first user-list))
               (length (compare-availability a-db user (first user-list)))
               (second user-list))) 
       (user-pendings a-db user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns requests made of the user formatted for a table
(define (make-request-table a-db user)
  (printf "Request-Table-User: ~S~n" (value-now user))
  (map (lambda (user-number-list)
         (list
          (user-first-name (first user-number-list))
          (user-last-name (first user-number-list))
          (length (compare-availability a-db user (first user-number-list)))
          (second user-number-list)
          (cons "Accept" user-number-list))) 
       (user-requests a-db user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-candidate-table a-db user)
  (printf "Candidate-Table-User: ~S~n" (value-now user))
  (map (lambda (usr)
         (list
          (user-first-name usr)
          (user-last-name usr)
          (length (compare-availability a-db user usr))
          (build-list 3 (lambda (x) (list (add1 x) usr)))))
       (sort-availability a-db user 
                          (user-candidates a-db user))))

(define init-db
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
    (2 1 4)]))


(define-values (user the-db #;profile-header)
  (letrec ([1-request-2-button (new ft-button%
                                    [label "1 request 2"]
                                    [parent vt-profile-pane])]
           [2-accept-1-button (new ft-button%
                                   [label "2 accept 1"]
                                   [parent vt-profile-pane])]
           [user-one (user-login init-db "1" "1")]
           [user-two (user-login init-db "2" "2")]
           [the-db (collect-b
                    (merge-e
                     (map-e (lambda (_) '1req2) (send 1-request-2-button get-value-e))
                     (map-e (lambda (_) '2acc1) (send 2-accept-1-button get-value-e)))
                    init-db
                    (lambda (event cur-db)
                      (case event
                        [(1req2) (request-partner! cur-db 
                                                   user-one 
                                                   user-two 
                                                   1)]
                        [(2acc1) (remove-request! 
                                  (remove-request! 
                                   (accept-request! cur-db user-two user-one 1) 
                                   user-one 1) 
                                  user-two 1)])))]       
           [login-email (new ft-text-field%
                             [label "email"]
                             [parent vt-profile-pane]
                             [init-value "1"])]
           [login-password (new ft-text-field%
                                [label "password"]
                                [parent vt-profile-pane]
                                [init-value "1"])]
           [login-button (new ft-button%
                              [label "login"]
                              [parent vt-profile-pane])]
           [_1 (printf "Before~n")]
           [user 
            (hold (changes
                   (user-login the-db 
                        (send login-email get-value) 
                        (send login-password get-value)))
                  user-one)
            
            #;(user-login the-db 
                        (send login-email get-value) 
                        (send login-password get-value))]
           [_2 (printf "After: ~S~n" (value-now user))]
           [_3 (new ft-text-field%
                    [label "user"]
                    [parent vt-profile-pane]
                    [init-value "Foo" #;(format "~S" (undefined? user))])]
           [team-table
            (make-team-table the-db user)]
           [pending-table
            (make-pending-table the-db user)]
           [request-table
            (make-request-table the-db user)]
           [candidate-table
            (make-candidate-table the-db user)]
           [profile-header (new ft-message% 
                                [label (format "Profile for ~a ~a" (user-first-name user) (user-last-name user))]
                                [parent vt-profile-pane]
                                [font a-font])]
           [request-button (new ft-button%
                                [label "Manage Requests"]
                                [parent vt-profile-pane])]
           [availability-button (new ft-button%
                                     [label "Change Availability"]
                                     [parent vt-profile-pane])]
           [which-pane? (hold 
                         (merge-e 
                          (map-e (lambda (_) 'requests) (send request-button get-value-e))
                          (map-e (lambda (_) 'availability) (send availability-button get-value-e)))
                         'requests)]
           [requests-enabled (eq? 'requests which-pane?)]
           [availability-enabled (eq? 'availability which-pane?)]
           [vt-request-pane (new vertical-pane% 
                                 [parent hl-pane] 
                                 [alignment '(left top)])]
           [vt-availability-pane (new vertical-pane% 
                                      [parent hl-pane] 
                                      [alignment '(left top)])]
           [request-message (new ft-message%
                                 [label "Requests"]
                                 [parent vt-request-pane]
                                 [font a-font]
                                 [enabled requests-enabled])]
           [team-message (new ft-message%
                              [label "Teams"]
                              [parent vt-request-pane]
                              [font a-font]
                              [enabled requests-enabled])]
           [team-headers-message (new ft-message%
                                      [label "Firstname   Lastname   Matching Freetime   Team Number"]
                                      [parent vt-request-pane]
                                      [enabled requests-enabled])]
           [test-1
            (new ft-message%
                 [label (apply string-append 
                               (map
                                (lambda (usr) 
                                  (format "~a    ~a    ~a    ~a~n"
                                          (first usr)
                                          (second usr)
                                          (third usr)
                                          (fourth usr)))
                                team-table))]
                 [parent vt-request-pane]
                 [enabled requests-enabled])]
           [pending-message (new ft-message%
                                 [label "Response Pending"]
                                 [parent vt-request-pane]
                                 [font a-font]
                                 [enabled requests-enabled])]
           [pending-headers-message (new ft-message%
                                         [label "Firstname   Lastname   Matching Freetime   Team Number"]
                                         [parent vt-request-pane]
                                         [enabled requests-enabled])]
           [test-2
            (new ft-message%
                 [label (apply string-append 
                               (map
                                (lambda (usr) 
                                  (format "~a    ~a    ~a    ~a~n" 
                                          (first usr)
                                          (second usr)
                                          (third usr)
                                          (fourth usr)))
                                pending-table))]
                 [parent vt-request-pane]
                 [enabled requests-enabled])]
           [requested-message (new ft-message%
                                   [label "Requested You"]
                                   [parent vt-request-pane]
                                   [font a-font]
                                   [enabled requests-enabled])]
           [requested-headers-message (new ft-message%
                                           [label "Firstname   Lastname   Matching Freetime   Team Number   Accept Request"]
                                           [parent vt-request-pane]
                                           [enabled requests-enabled])]
           [test-3
            (new ft-message%
                 [label (apply string-append 
                               (map
                                (lambda (usr)
                                  (format "~a    ~a    ~a    ~a~n" 
                                          (first usr)
                                          (second usr)
                                          (third usr)
                                          (fourth usr)
                                          #;(fifth usr)))
                                request-table))]
                 [parent vt-request-pane]
                 [enabled requests-enabled])]
           [candidate-message (new ft-message%
                                   [label "Partners Available"]
                                   [parent vt-request-pane]
                                   [font a-font]
                                   [enabled requests-enabled])]
           [candidate-headers-message (new ft-message%
                                           [label "Firstname   Lastname   Matching Freetime   Team Number  Request"]
                                           [parent vt-request-pane]
                                           [enabled requests-enabled])]
           [test-4
            (new ft-message%
                 [label (apply string-append 
                               (map
                                (lambda (usr) 
                                  (format "~a    ~a    ~a    ~a~n" 
                                          (first usr)
                                          (second usr)
                                          (third usr)
                                          (fourth usr)
                                          #;(fifth usr)))
                                candidate-table))]
                 [parent vt-request-pane]
                 [enabled requests-enabled])]
           [availability-message (new ft-message%
                                      [label "Add Times You Are Available"]
                                      [parent vt-availability-pane]
                                      [font a-font]
                                      [enabled availability-enabled])])
    (values user the-db #;profile-header)))
