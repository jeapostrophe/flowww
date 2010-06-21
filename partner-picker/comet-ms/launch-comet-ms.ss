#lang mzscheme
(require web-server/servlet-env
         "control-comet-ms.ss"
         "../original/random-db.ss")

; Go!
(define start (make-start random-picker))
(serve/servlet start
               ;#:extra-files-paths (list (this-expression-source-directory))
               )