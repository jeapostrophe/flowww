#lang frtime
(require "../frfx.ss")

(define (start req)
  `(html (head (title "FrWeb Time Test"))
         (body ()
               (h2 "The current time is: "
                   ,(number->string seconds))
               (h2 ,(if (even? seconds)
                        `(span ([style "color: red"]) "That's even, by the way.")
                        `(i "Unfortunately, it's not even."))))))

(serve/frp start)