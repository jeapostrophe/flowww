#lang scheme
(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         "db-tests.ss"
         "control-tests.ss"
         "model-tests.ss"
         "view-tests.ss")

(run-tests 
 (test-suite
  "Partner Picker"
  
  db-tests
  model-tests
  control-tests
  view-tests))