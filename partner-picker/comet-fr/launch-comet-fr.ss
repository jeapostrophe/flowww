#lang scheme
(require web-server/servlet-env
         "../../frweb/frfx.ss"
         mzlib/etc
         "control-comet-fr.ss"
         "../original/random-db.ss"
         web-server/managers/timeouts)

; Go!
(define start (make-start random-picker))
(serve/servlet (wrap-start start)
           #:manager 
           (create-timeout-manager
            (lambda (req)
              `(html (body (h1 "Expired. =("))))
            +inf.0 +inf.0)
           #:extra-files-paths (list flowww-home (this-expression-source-directory))
               )