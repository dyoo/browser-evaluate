#lang setup/infotab
(define name "browser-evaluate")
(define categories '(devtools))
(define can-be-loaded-with 'all)
(define required-core-version "5.1.2")
(define version "1.0")
(define repositories '("4.x"))
(define scribblings '(("manual.scrbl" ())))
(define primary-file "main.rkt")
(define blurb 
  '((p "browser-evaluate: evaluate JavaScript expressions in the browser through Racket.")))
(define release-notes
  '((p "Initial release.")))