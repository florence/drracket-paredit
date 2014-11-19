#lang racket
(provide test)
(require (for-syntax syntax/parse) rackunit paredit/private/paredit framework)

(define-syntax (test stx)
  (syntax-parse stx
    #:datum-literals (=>)
    [(test before after (op ...)
           => result)
     #'(test-begin
        (define t (new (paredit-text-mixin racket:text%)))
        (define first before)
        (define rest after)
        (send t insert first)
        (send t insert rest)
        (send t set-position (string-length first))
        (send t op ...)
        (check-equal? (send t get-text)
                      result))]))
