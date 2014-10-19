#lang racket/unit

(require
 (for-syntax syntax/parse)
 racket/gui drracket/tool framework)

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
(define paredit-text-mixin
  (lambda (%)
    (class* % ()
      (inherit insert
               get-position
               position-line
               line-start-position
               line-end-position)

      (define paredit? (preferences:get pref-key))
      (define/public-final (toggle-paredit!)
        (set! paredit? (not paredit?))
        (preferences:set pref-key paredit?))
      (define/public-final (check-paredit?) paredit?)


      (define/override (on-local-char evt)
        (cond [paredit?
               (define key (send evt get-key-code))
               (cond
                [(char-pair-right? key) =>
                 (lambda (p)
                   (insert-pair . p))]
                [(char-pair-left? key) =>
                 ;;todo paren navigation
                 void]
                [else
                 (match key
                   #;
                   [#\backspace
                    (do-balanced-delete)]
                   [else (super on-local-char evt)])])]
              [else (super on-local-char evt)]))

      (define-syntax char
        (lambda (stx)
          (syntax-parse stx
            [(_ s:str)
             (with-syntax ([c (string-ref (syntax-e #'s) 0)])
               #'c)])))
      

      (define/private (insert-pair left right)
        (define-values (start _end) (get-current-line-start-end))
        (send this insert left start)
        (send this insert right (add1 start)))
        
      (define char-pairs
        `((,(char "(") ,(char ")"))
          (,(char "[") ,(char "]"))
          (,(char "{") ,(char "}"))
          (,(char "|") ,(char "|"))))
      (define r-char-pairs
        (map reverse char-pairs))
      
      (define (char-pair-right? s)
        (assoc s char-pairs))
      (define (char-pair-left? s)
        (assoc s r-char-pairs))

      ;; -> (values int int)
      ;; gets the start and end position of the line at the start of current selection
      (define/private (get-current-line-start-end)
        (define b (box 0))
        (get-position b)
        (define line (position-line (unbox b)))
        (values (line-start-position line)
                (line-end-position line)))
      
      (super-new))))

;;setup 
(preferences:set-default pref-key #t boolean?)
(drracket:get/extend:extend-definitions-text paredit-text-mixin)
