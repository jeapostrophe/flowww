#lang frtime
(require "../frfx.ss")

(define (display-time who)
  (display-time
   (extract-binding/single
    'who
    (request-bindings
     (send/suspend
      (lambda (k-url)
        `(html (head (title "FrWeb Status Test"))
               (body ()
                     ,@(if who `((h1 "Hello " ,who)) empty)
                     (h2 "The current time is: "
                         ,(number->string seconds))
                     (h2 ,(if (even? seconds)
                              `(span ([style "color: red"]) "That's even, by the way.")
                              `(i "Unfortunately, it's not even.")))
                     (p (form ([action ,k-url])
                              "My name is " (input ([name "who"]))))))))))))

(define (start req)
  (display-time #f))

(serve/frp start)