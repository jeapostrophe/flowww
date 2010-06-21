#lang frtime
(require (lifted texpict/mrpict colorize vl-append vr-append text
                 cc-superimpose hb-append ht-append pin-over blank
                 dc-for-text-size)
         (lifted scheme/base make-string build-list)
         (lifted texpict/utils filled-rectangle rectangle)
         frtime/gui/fred mred
         (only frtime/frp-core do-in-manager do-in-manager-after)
         (lifted (planet jaymccarthy/fra:2:0) relation-schema database-insert
                 execute-query query-projection query-relation)
         (only (planet jaymccarthy/fra:2:0) Database Tuple with-database)
         (lifted "print-rel.ss" print-relation)
         (lifted scheme/port with-output-to-string))
(provide print-relation)

(define sizing-dc
  (new bitmap-dc% [bitmap (make-object bitmap% 64 64)]))

(dc-for-text-size sizing-dc)
(do-in-manager
 (dc-for-text-size sizing-dc))

(define size (new-cell 20))

(do-in-manager-after ())

(define frame (new ft-frame% [label "Guest Manager"] [shown #t]
                   [min-width (* size 50)] [min-height (* size 50)]))

(define a-font (make-object font% 16 'roman 'normal 'bold))

(define init-guestlistDB
  (Database
   [Guests
    [guestId name]
    (0 "Joseph")
    (1 "Brigham")
    (2 "John")]))

(define (relation->string r)
  (with-output-to-string
   (lambda () (print-relation r))))

(define cur-id 2)

(define (next-id)
  (begin
    (set! cur-id (add1 cur-id))
    cur-id))

(letrec ([new-guest (new ft-text-field%
                         [label "new guest"]
                         [parent frame]
                         [init-value "1"])]
         [new-name-b (send new-guest get-value-b)]
         [add-button (new ft-button%
                          [label "add guest"]
                          [parent frame])]
         [guestlistDB (collect-b
                             (send add-button get-value-e)
                             init-guestlistDB
                             (lambda (evt the-db)
                               (database-insert the-db
                                                'Guests
                                                (Tuple (next-id) (value-now new-name-b)))))]
         [guests (new ft-message% 
                      [label (hold 
                              (==> (changes guestlistDB)
                                  (lambda (the-db)
                                    (with-database the-db
                                          (relation->string 
                                           (execute-query 
                                            (query-projection
                                             '(name)
                                             (query-relation 'Guests)))))))
                              (apply string-append
                                     (build-list 10
                                                 (lambda _
                                                   "aaaaaaaaaa\n"))))
                              ]
                      [parent frame]
                      [font a-font])])
  (void))