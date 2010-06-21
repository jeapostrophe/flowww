#lang frtime
(require "../frfx.ss")

(define (start req)
  `(html (head (title "FrWeb Time Test"))
         (body (h2 "The current time is: "
                   ,(number->string seconds))
               (h2 ,(if (even? seconds)
                        `(span ([style "color: red"]) "That's even, by the way.")
                        `(i "Unfortunately, it's not even.")))
               (ul (li "Top")
                   ,@(map
                      (lambda (i)
                        `(li ,(number->string i)))
                      (build-list (modulo seconds 10) add1)))
               (ul ,@(map
                      (lambda (i)
                        `(li ,(number->string i)))
                      (build-list (modulo seconds 10) add1)))
               #;(,(if (even? seconds)
                     'h2
                     'h3)
                "Some header")
               #;(span ([style ,(if (even? seconds)
                                  "color: red"
                                  "color: blue")])
                     "Some text")
               ; XXX attr name behavior
               #;(span (,(if (even? seconds)
                                  '[style "color: red"]
                                  '[style "color: blue"]))
                     "Some text")
               #;(span ,(if (even? seconds)
                                  '([style "color: red"])
                                  '([style "color: blue"]))
                     "Some text"))))

(serve/frp start)