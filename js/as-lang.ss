#lang scheme
(require "as.ss")
(provide as-program)

(define-syntax (as-program stx)
  (syntax-case stx ()
    [(_ p ...)
     (syntax/loc stx
       (as-program->js-program '(p ...)))]))