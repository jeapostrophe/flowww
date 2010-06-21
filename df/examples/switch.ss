#lang scheme
(require "../df.ss")

(reactimate! 
 (make-switch
  (lift even? seconds-evt)
  (lift + 3 seconds-evt)
  (lift + 4 seconds-evt)))