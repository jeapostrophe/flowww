#lang mzscheme

(require (planet schematics/schemeunit:3)
         "kill-safe-ms.ss"
         "../comet-fr/model-fr.ss")

(require/expose "../comet-fr/control-comet-fr.ss" (user-hash))


(provide handle-request-evt
         state-change-evt
         )

(define handle-request-evt
  (lambda (st ur)
    (guard-evt
     (lambda ()
       (if (waiting? st ur)
           never-evt
           (wrap-evt always-evt
                     (lambda (_)
                       (printf "~S done waiting~n" ur)
                       (make-state+data st 'ok))))))))

(define state-change-evt
  (lambda (st)
    (wrap-evt db-change-notify-ch
              (lambda (update-time)
                (cond
                  [(server-state? st)
                   (if (<= update-time (server-state-update-time st))
                       st
                       (make-server-state (server-state-update-time st) (make-hash-table)))])))))

(define (waiting? st req)
  (cond
    [(update-request? req)
     (bytes=? (update-request-old-data req)
              (hash-ref! (server-state-cache st) (user-email (update-request-user req))
                         (lambda () (user-hash (update-request-user req)))))]))

(define db-change-notify-ch
  (make-async-channel))


