#lang frtime

(require web-server/http/request-structs
         (lifted mzscheme 
                 regexp-replace*)
         (lifted scheme/base
                 bytes->string/utf-8
                 make-hash
                 hash-ref!)
         "js/js.ss"
         "frweb/frfx.ss")

(provide post-data->string 
         binding->string
         escape-string 
         quote-string
         func-arg-evt 
         func-arg-bhv
         changed
         flowww-script 
         flowww-handler 
         flowww-post
         raise-reactivity
         value-now)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persistant store functional interaction helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hash to track the signals for the persistant store updates
(define signal-ht (make-hash))

;; signal-value: any (any/c->event-receiver) -> event-receiver
;; Returns event-receiver at the hash of n. If no hash exists,
;; calls init-thunk stores the result at hash of n then returns
;; the result.
(define (signal-value n init-thunk)
  (hash-ref! signal-ht n init-thunk))


(define (func-arg-evt f . args)
  (signal-value (list* f args)
                (lambda () (event-receiver))))

(define ((func-arg-bhv f) . args)
  (hold (map-e (lambda (v) (apply f args))
               (apply func-arg-evt f args))
        (apply f args)))

(define (changed event)
  (send-event event 'changed))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Request data string converting helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; post-data->string: request -> String
;; turns all post data into a utf-8 string
(define (post-data->string request)
  (define post (request-post-data/raw request))
  (bytes->string/utf-8
   post))

;; binding->string: id request -> String
;; throws an error if binding was not recieved
(define (binding->string id request)
  (define binds (request-bindings/raw request))
  (define b (bindings-assq id binds))
  (if (binding:form? b)
      (bytes->string/utf-8
       (binding:form-value b))
      (error 'binding->string "Binding not received")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Url formatting helper functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; escape-string: string -> string
;; formats a string to be used as javscript onload urls
(define (escape-string s)
  (regexp-replace* "%20" s " "))

;; quote-string: string -> string
;; formats a string to be used as javscript href urls
(define (quote-string s)
  (regexp-replace* "%22" s "%5C%22"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript functions needed to perform the ajax function calls ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax flowww-post
  (syntax-rules ()
    [(flowww-post url post-data)
     (javascript: (sendPost 
                   #,(quote-string 
                      url)
                   post-data))]))

(define flowww-script
  (javascript
   (var xmlHttp)
   (function 
    sendPost (url PostData)
    (try 
     (block
      (= xmlHttp (new XMLHttpRequest)))
     (catch exn
       (block
        (try
         (block
          (= xmlHttp (new ActiveXObject "Msxml2.XMLHTTP")))
         (catch exn
           (block
            (try
             (block
              (= xmlHttp 
                 (new ActiveXObject "Microsoft.XMLHTTP")))
             (catch exn
               (block
                (alert 
                 "Unable to create an XMLHTTP instance"))))))))))
    ((field xmlHttp open) "POST" url #t)
    ((field xmlHttp send) PostData))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make-url Handler Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flowww-handler thunk)
     (lambda (request)
       (define data 
         (post-data->string request))
       (thunk data)
       (send/back `(html 
                    (head) 
                    (body () "Flowww Default Response")))))
