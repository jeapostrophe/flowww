#lang scheme
(require "../df.ss")

(reactimate! 
 (lift < seconds-evt
       (lift add1 seconds-evt)))