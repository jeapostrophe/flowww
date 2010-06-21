#lang scheme
(require web-server/servlet-env
         mzlib/etc
         "control-comet.ss"
         "../original/random-db.ss")

; Go!
(define start (make-start random-picker))
(serve/servlet start
               ;#:listen-ip "192.168.25.45"
               ;#:servlet-path "/"
               #:extra-files-paths (list (this-expression-source-directory)))