#lang frtime
(require scheme/contract)
(provide/contract
 [every (number? . -> . event?)]
 [watch (event? (-> any) . -> . signal?)])

(define (every interval)
  (when-e (zero? (modulo seconds interval))))

(define (watch event thnk)
  (hold
   (map-e 
    (lambda _
      (thnk))
    event)
   (thnk)))