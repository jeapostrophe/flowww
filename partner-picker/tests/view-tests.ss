#lang scheme
(require (planet schematics/schemeunit:3)
         "view.ss"
         "model.ss")
#;(require (planet untyped/delirium/delirium))

(provide view-tests)

(define view-tests
  (test-suite
   "Tested by hand"))
