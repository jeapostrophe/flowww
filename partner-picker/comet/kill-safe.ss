#lang scheme
(define state/c any/c)
(define request-data/c any/c)
(define response-data/c any/c)
(define (evt/c sync/c) evt?)

(define-struct server (thread request-ch))
(define-struct msg (reply-ch partner-dead-evt data))
(define-struct state+data (state data))
(provide/contract
 [server? (any/c . -> . boolean?)]
 [struct state+data 
         ([state state/c]
          [data response-data/c])])

(provide/contract
 [kill-safe-server (-> #:accept-new-requests? (state/c . -> . boolean?)
                       #:handle-request-evt (state/c request-data/c . -> . (evt/c state+data?))
                       #:register-new-request (state/c request-data/c . -> . state/c)
                       #:register-response-delivery (state/c response-data/c . -> . state/c)
                       #:register-request-failure (state/c request-data/c . -> . state/c)
                       #:register-response-failure (state/c response-data/c . -> . state/c)
                       #:state-change-evt (state/c . -> . (evt/c state/c))
                       #:initial-state state/c
                       server?)])

(define (kill-safe-server #:accept-new-requests? accept-new-requests?
                          #:handle-request-evt handle-request-evt
                          #:register-new-request register-new-request
                          #:register-response-delivery register-response-delivery
                          #:register-request-failure register-request-failure
                          #:register-response-failure register-response-failure
                          #:state-change-evt state-change-evt
                          #:initial-state initial-state)
  (define request-ch (make-channel))
  (define (server-loop #:active-requests active-requests
                       #:completed-requests completed-requests
                       #:state state)
    (apply sync
           ; Allow the state to change
           (handle-evt (state-change-evt state)
                       (lambda (new-state)
                         (server-loop #:active-requests active-requests
                                      #:completed-requests completed-requests
                                      #:state new-state)))
           ; Wait for new requests
           (if (accept-new-requests? state)
               (handle-evt request-ch
                           (lambda (req)
                             (server-loop #:active-requests (list* req active-requests)
                                          #:completed-requests completed-requests
                                          #:state (register-new-request state (msg-data req)))))
               never-evt)
           (append
            ; Wait for a request to be handled
            (map (lambda (req)
                   (handle-evt (handle-request-evt state (msg-data req))
                               (match-lambda
                                 [(struct state+data (new-state response))
                                  (server-loop #:active-requests
                                               (remq req active-requests)
                                               #:completed-requests 
                                               (list* (struct-copy msg req
                                                                   [data response])
                                                      completed-requests)
                                               #:state
                                               new-state)])))
                 active-requests)
            ; Wait for a reply to be received
            (map (match-lambda
                   [(and reply (struct msg (reply-ch _ data)))
                    (handle-evt (channel-put-evt reply-ch data)
                                (lambda (_)
                                  (server-loop #:active-requests active-requests
                                               #:completed-requests (remq reply completed-requests)
                                               #:state (register-response-delivery state data))))])
                 completed-requests)
            ; Check if the partner is dead or has given up
            (local [(define ((check-if-partner-dead? remove-msg) msg)
                      (handle-evt (msg-partner-dead-evt msg)
                                  (lambda (_)
                                    (remove-msg msg))))]                      
              (map (check-if-partner-dead?
                    (lambda (req)
                      (server-loop #:active-requests (remq req active-requests)
                                   #:completed-requests completed-requests
                                   #:state (register-request-failure state (msg-data req)))))
                   active-requests)
              (map (check-if-partner-dead?
                    (lambda (req)
                      (server-loop #:active-requests active-requests
                                   #:completed-requests (remq req completed-requests)
                                   #:state (register-response-failure state (msg-data req)))))
                   completed-requests)))))
  (define kill-safe-server-t
    (thread 
     (lambda ()
       (server-loop #:active-requests empty
                    #:completed-requests empty
                    #:state initial-state))))
  (make-server kill-safe-server-t request-ch))

(provide/contract
 [kill-safe-request-evt (-> server? request-data/c
                            (evt/c response-data/c))])
(define (kill-safe-request-evt server data)
  (nack-guard-evt
   (lambda (gave-up-evt)
     (define reply-ch (make-channel))
     ; Make sure the thread is running...
     (thread-resume (server-thread server) (current-thread))
     ; Make the request
     (channel-put (server-request-ch server)
                  (make-msg reply-ch
                            ; The server should abandon us if we give up or if we die
                            (choice-evt gave-up-evt 
                                        (thread-dead-evt (current-thread)))
                            data))
     ; Return the evt that will wait for the reply
     reply-ch)))

(provide/contract
 [kill-safe-request (-> server? request-data/c
                        response-data/c)])
(define (kill-safe-request server data)
  (sync (kill-safe-request-evt server data)))