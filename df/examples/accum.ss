#lang scheme
(require "../df.ss")

(reactimate!
 (collect seconds-evt
          0
          (lambda (s v)
            (add1 v))))