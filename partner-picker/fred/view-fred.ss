(require (lifted texpict/mrpict colorize vl-append vr-append text
                 cc-superimpose hb-append ht-append pin-over blank
                 dc-for-text-size)
         (lifted texpict/utils filled-rectangle rectangle)
         frtime/gui/fred mred
         (only frtime/frp-core do-in-manager do-in-manager-after)
         "random-db-fr.ss"
         (lifted "model-fr.ss" user-first-name user-last-name compare-availability 
                 ft-user-partners ft-user-pendings ft-user-requests ft-user-candidates sort-availability
                 user-login request-partner! accept-request! ft-remove-request! make-user)
         "../frtime/watch.ss")

(define sizing-dc
  (new bitmap-dc% [bitmap (make-object bitmap% 64 64)]))

(dc-for-text-size sizing-dc)
(do-in-manager
 (dc-for-text-size sizing-dc))

(define size (new-cell 20))

(do-in-manager-after ())

(define frame (new ft-frame% [label "Partner Picker"] [shown #t]
                   [min-width (* size 50)] [min-height (* size 50)]))

(define a-font (make-object font% 16 'roman 'normal 'bold))

(define hl-pane (new horizontal-pane% 
                     [parent frame] 
                     [alignment '(left top)]))

(define vt-profile-pane (new vertical-pane% 
                             [parent hl-pane] 
                             [alignment '(left top)]))         

(define (make-team-table user)
  (map (lambda (user-list)
         (list (user-first-name (first user-list))
               (user-last-name (first user-list))
               (length (compare-availability user (first user-list)))
               (second user-list)))
       (ft-user-partners user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-pending-table user)
  (map (lambda (user-list)
         (list (user-first-name (first user-list))
               (user-last-name (first user-list))
               (length (compare-availability user (first user-list)))
               (second user-list)))
       (ft-user-pendings user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns requests made of the user formatted for a table
(define (make-request-table user)
  (map (lambda (user-number-list)
         (list
          (user-first-name (first user-number-list))
          (user-last-name (first user-number-list))
          (length (compare-availability user (first user-number-list)))
          (second user-number-list)
          (cons "Accept" user-number-list))) 
       (ft-user-requests user)))

;; make-team-table: user -> (listof list)
;; Takes the user and returns partners formatted for a table
(define (make-candidate-table user)
  (map (lambda (usr)
         (list
          (user-first-name usr)
          (user-last-name usr)
          (length (compare-availability user usr))
          (build-list 3 (lambda (x) (list (add1 x) usr)))))  
       (sort-availability user (ft-user-candidates user))))

(define user-one (user-login random-picker "1" "1"))

(define user-two (user-login random-picker "2" "2"))

;(define-values (user profile-header)
(letrec ([login-email (new ft-text-field%
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
         [printed (map-e (lambda (_) (printf "~n~a"(send login-email get-value))) 
                         (send login-button get-value-e))]
         [user (hold
                (map-e (lambda (_)
                         (user-login random-picker (send login-email get-value) (send login-password get-value))) 
                       (send login-button get-value-e))
                user-one)]       
         [team-table
          (make-team-table user)]
         [pending-table
          (make-pending-table user)]
         [request-table
          (make-request-table user)]
         [candidate-table
          (make-candidate-table user)]
         [profile-header (new ft-message% 
                              [label (format "Profile for ~a ~a" 
                                             (user-first-name user)
                                             (user-last-name user))]
                              [parent vt-profile-pane]
                              [font a-font])]
         [request-button (new ft-button%
                              [label "Manage Requests"]
                              [parent vt-profile-pane])]
         [availability-button (new ft-button%
                                   [label "Change Availability"]
                                   [parent vt-profile-pane])]
         [1-request-2-button (new button%
                                  [label "1 request 2"]
                                  [parent vt-profile-pane]
                                  [callback (lambda (b e)
                                              (request-partner! user-one user-two 1))])]
         [2-accept-1-button (new button%
                                 [label "2 accept 1"]
                                 [parent vt-profile-pane]
                                 [callback (lambda (b e)
                                             (accept-request! user-two user-one 1)
                                             (remove-request! user-one 1)
                                             (remove-request! user-two 1))])]
         [which-pane? (hold 
                       (merge-e 
                        (map-e (lambda (_) 'requests) (send request-button get-value-e))
                        (map-e (lambda (_) 'availability) (send availability-button get-value-e)))
                       'requests)]
         [requests-enabled (equal? 'requests which-pane?)]
         [availability-enabled (equal? 'availability which-pane?)]
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
         #;[test-4
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
                                    [enabled availability-enabled])]))
;(values user profile-header)))
