#lang scheme
(require web-server/servlet
         (only-in frtime/frtime
                  event-receiver
                  event? behavior? hold
                  raise-reactivity send-synchronous-event
                  deep-value-now)
         net/url)
(provide wrap-start)

(define (wrap-start input-e->output-e)
  (define input-request-e (event-receiver))  
  (define output-response
    (input-e->output-e input-request-e))  
  (define connector
    (cond
      [(event? output-response)
       (hold output-response "Uninitialized.")]
      [(behavior? output-response)
       output-response]
      [else
       (raise-reactivity output-response)]))
  (define (start ireq)
    (send-synchronous-event input-request-e ireq)
    (send/back (deep-value-now connector)))
  start)