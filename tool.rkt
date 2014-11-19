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

(define paredit-frame-mixin
  (lambda (%)
    (class % (drracket:unit:frame<%>)
      (inherit get-defintions-text)
      
      (define/override (edit-menu:between-find-and-preferences menu)
        (super edit-menu:between-find-and-preferences menu)
        (new checkable-menu-item%
             [label "Paredit Mode"]
             [parent menu]
             [callback 
              (lambda (i e) (send (get-defintions-text) toggle-paredit!))]
             [checked (send (get-defintions-text) check-paredit?)]))
      
      (super-new))))

;;setup 
(preferences:set-default pref-key #t boolean?)
(drracket:get/extend:extend-definitions-text paredit-text-mixin)

