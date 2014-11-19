#lang setup/infotab

(define drracket-tools '(("tool.rkt")))
(define drracket-tool-names (list "Paredit Mode"))
(define drracket-tool-icons '(#f))

(define blurb '("Paredit mode for DrRacket"))
(define catagories '(devtools))
(define primary-file "tool.rkt")

(define deps
  '("base" "gui-lib" "drracket-plugin-lib"))
(define build-deps
  '("rackunit-lib"))

(define single-collection "paredit")
