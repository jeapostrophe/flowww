#lang frtime
(require (prefix sqlite: (planet jaymccarthy/sqlite:4/sqlite))
         "../frweb.ss"
         "../../frtime/watch.ss"
         (lifted scheme/list rest)
         web-server/http/bindings
         web-server/servlet-env)

(define db (sqlite:open ':memory:))
(sqlite:exec/ignore db "CREATE TABLE user ( user_id INTEGER PRIMARY KEY, name TEXT )")
(sqlite:insert db "INSERT INTO user (name) VALUES ('joseph')")
(sqlite:insert db "INSERT INTO user (name) VALUES ('brigham')")
(sqlite:insert db "INSERT INTO user (name) VALUES ('john')")

(define db-update! (event-receiver))

(define users
  (map (lambda (v) (vector-ref v 0))
       (rest (watch (merge-e (every 10)
                             db-update!)
                    (lambda () (sqlite:select db "SELECT name FROM user"))))))

(define (start input-request-event)
  (input-request-event
   . ==> .
   (lambda (ir)
     (define binds (request-bindings ir))
     (when (exists-binding? 'new binds)
       (sqlite:insert 
        db 
        (format "INSERT INTO user (name) VALUES ('~a')"
                (extract-binding/single 'new binds)))
       (send-event db-update! #t))
     `(html (head (title "FrWeb DB Read Test"))
            (body
             (h2 "Users: "
                 (ul ,@(map (lambda (u) `(li ,u))
                            users)))
             (form (input ([name "new"]))
                   (input ([type "submit"]))))))))

(serve/servlet (wrap-start start))         