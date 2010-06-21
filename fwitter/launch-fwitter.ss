#lang scheme
(require web-server/servlet-env
         (planet byu-plt/flowww:1:0)
         "control-fwitter.ss")

; Go!

(create-user "1")
(create-user "2")
(create-user "3")
(create-user "4")
(create-user "5")
(create-user "6")

(follow-user! (get-user "1") (get-user "3"))
(follow-user! (get-user "2") (get-user "5"))
(follow-user! (get-user "4") (get-user "3"))
(follow-user! (get-user "1") (get-user "6"))
(follow-user! (get-user "5") (get-user "2"))
(follow-user! (get-user "6") (get-user "3"))
(follow-user! (get-user "3") (get-user "1"))
(follow-user! (get-user "2") (get-user "4"))
(follow-user! (get-user "1") (get-user "2"))
(follow-user! (get-user "4") (get-user "5"))
(follow-user! (get-user "3") (get-user "6"))
(follow-user! (get-user "6") (get-user "2"))

(user-status-update! (get-user "1") "q")
(user-status-update! (get-user "2") "w")
(user-status-update! (get-user "3") "e")
(user-status-update! (get-user "4") "r")
(user-status-update! (get-user "5") "t")
(user-status-update! (get-user "6") "y")

(define start (make-start))
(serve/frp (wrap-start start))