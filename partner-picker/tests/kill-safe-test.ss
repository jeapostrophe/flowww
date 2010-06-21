#lang scheme
(require "kill-safe.ss")

(define KILL-CHANCE 5)
(define MAX 50)
(define THREADS 50)
(define REQS 50)
(define KILL-WAIT 800)
(define WAITING 1000)

(define-struct ping-req (timer) #:transparent)

(define ping-server
  (kill-safe-server #:accept-new-requests? 
                    (lambda (n) 
                      (printf "[~a] Accept new requests?~n" n)
                      (n . < . MAX))
                    #:handle-request-evt 
                    (lambda (n d)
                      (wrap-evt (alarm-evt (+ WAITING (ping-req-timer d)))
                                (lambda (_)
                                  (printf "[~a] Request completed: ~S~n" n d)
                                  (make-state+data n (current-inexact-milliseconds)))))
                    #:register-new-request
                    (lambda (n d)
                      (printf "[~a] New request: ~S~n" n d)
                      (add1 n))
                    #:register-response-delivery 
                    (lambda (n d)
                      (printf "[~a] Completed response ~S~n" n d)
                      (sub1 n))
                    #:register-request-failure
                    (lambda (n d)
                      (printf "[~a] Failed request: ~S~n" n d)
                      (sub1 n))
                    #:register-response-failure 
                    (lambda (n d)
                      (printf "[~a] Failed response ~S~n" n d)
                      (sub1 n))
                    #:state-change-evt
                    (lambda (n)
                      never-evt)
                    #:initial-state 0))

(define (ping-request)
  (define start
    (current-inexact-milliseconds))
  (define end
    (kill-safe-request ping-server (make-ping-req start)))
  (- end start))

(define (wait-for-all-threads ts)
  (unless (empty? ts)
    (when (= 0 (random KILL-CHANCE))
      (printf "Killing!~n")
      (kill-thread (list-ref ts (random (length ts)))))
    (apply sync
           (handle-evt (alarm-evt (+ KILL-WAIT (current-inexact-milliseconds)))
                       (lambda (_)
                         (wait-for-all-threads ts)))
           (map (lambda (t)
                  (handle-evt (thread-dead-evt t)
                              (lambda (_)
                                (wait-for-all-threads (remq t ts)))))
                ts))))

(define all-threads
  (for/list ([i (in-range 0 THREADS)])
    (thread
     (lambda ()
       (for ([j (in-range 0 REQS)])
         (printf "Ping took: ~a~n" (ping-request)))))))

(wait-for-all-threads all-threads)