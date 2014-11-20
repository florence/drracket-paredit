#lang racket/unit

(require
 (for-syntax syntax/parse)
 racket/gui drracket/tool framework
 "private/paredit.rkt")

(import drracket:tool^)
(export drracket:tool-exports^)

(define pref-key 'drracket:paredit)
(define phase1 void)
(define phase2 void)

(define odisplayln
  (curryr displayln (current-output-port)))

(define paredit-frame-mixin
  (lambda (%)
    (class* % (drracket:unit:frame<%>)
      
      (define/override (edit-menu:between-find-and-preferences menu)
        (new checkable-menu-item%
             [label "Paredit Mode"]
             [parent menu]
             [callback 
              void #;(lambda (i e) (send (get-defintions-text) toggle-paredit!))
              ]
             [checked #t #;(send (get-defintions-text) check-paredit?)
              ])
        (super edit-menu:between-find-and-preferences menu))
      
      (super-new))))

;;setup 
(preferences:set-default pref-key #t boolean?)
(drracket:get/extend:extend-definitions-text paredit-text-mixin)
(drracket:get/extend:extend-unit-frame paredit-frame-mixin)


