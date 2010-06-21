#lang scheme
(require "../frweb.ss"
         "rtime.ss"
         scheme/runtime-path
         web-server/servlet-env)

(define-runtime-path floww-home "../../flapjax/fx")
(serve/servlet (wrap-start start)
               #:launch-browser? #t
               #:extra-files-paths
               (list floww-home))