#lang racket
(provide test)
(require (for-syntax syntax/parse) rackunit paredit/private/paredit framework)

(define-syntax (test stx)
  (syntax-parse stx
    #:datum-literals (=>)
    [(test before after (op ...)
           => result)
     (quasisyntax/loc stx
       (test-begin
        (define t (do-test-ops before after (op ...)))
        #,(syntax/loc stx
            (check-equal? (send t get-text)
                          result))))]
    [(test before after (op ...)
           => result-left:str result-right)
     (quasisyntax/loc stx
       (test-begin
        (define t (do-test-ops before after (op ...)))
        #,(syntax/loc stx
            (check-equal? (send t get-text)
                          (string-append result-left result-right)))
        #,(syntax/loc stx
            (check-equal? (send t get-start-position)
                          (string-length result-left)))))]))

(define-syntax (do-test-ops stx)
  (syntax-parse stx
    [(_ before after (op ...))
     #'(let ()
        (define t (new (paredit-text-mixin racket:text%)))
        (define first before)
        (define rest after)
        (send t insert first)
        (send t insert rest)
        (send t set-position (string-length first))
        (send t op ...)
        t)]))
