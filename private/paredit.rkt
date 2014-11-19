#lang racket
(provide paredit-text-mixin add-default-keys!)
(require (for-syntax syntax/parse)
         racket/gui
         drracket/tool
         framework)

(define odisplayln
  (curryr displayln (current-output-port)))

(define paredit-text-mixin
  (lambda (%)
    (class* % ()
      (inherit insert
               get-start-position
               move-position)

      ;; ===== public functions =====
      (define/public (insert-pair key)
        (cond [(in-string?)
               (insert/escape key)]
              [(in-comment?)
               (insert key)]
              [else
               (define right (last (assoc key char-pairs)))
               (insert-p key right)
               (move-position 'left)]))

      (define/public (do-balanced-delete)
        (cond
         [(in-string?) 
          (if (and (at-string-start?)
                   (before-after-pair? '((#\" #\"))))
              (matching-delete)
              (delete-one-char))]
         [(in-comment?) (delete-one-char)]
         [(before-after-pair?) 
          (matching-delete)]
         [else (delete-one-char)])) 

      ;; ===== private functions =====
      (define/private (insert-p left right)
        (define start (get-start-position))
        (insert left start)
        (insert right (add1 start)))
      
      (define/private (insert/escape char)
        (when (needs-escaping? char)
          (insert #\\))
        (insert char))
      
      (define/private (in-string?)
        (eq? 'string (classify-here)))
      (define/private (in-comment?)
        (define type (classify-here))
        (or (eq? 'comment type)
            (eq? 'sexp-comment type)))
      (define/private (classify-here)
        (send this classify-position (get-start-position)))
      
      ;; assumes (in-string?)
      (define/private (at-string-start?)
        (define-values (start _end) (send this get-token-range (get-start-position)))
        (= start (get-start-position)))

      (define/private (before-after-pair? [set char-pairs])
        (define pos (get-start-position))
        (define cleft (send this get-character (sub1 pos)))
        (define cright (send this get-character pos))
        (for/or ([pair (in-list set)])
          (define-values (left right) (apply values pair))
          (and (eq? left cleft)
               (eq? right cright))))

      (define/private (matching-delete) 
        (move-position 'right)
        (delete-one-char)
        (delete-one-char))
      
      (define/private (delete-one-char)
        (send this delete (get-start-position)))
     
      ;; ===== utilities =====
       (define char-pairs
        `((#\( #\))
          (#\[ #\])
          (#\{ #\})
          (#\| #\|)
          (#\" #\")))

      (define r-char-pairs
        (map reverse char-pairs))
      
      (define (char-pair-right? s)
        (assoc s char-pairs))
      (define (char-pair-left? s)
        (assoc s r-char-pairs))
      
      (define escape? (seteq #\"))
      (define (needs-escaping? k)
        (set-member? escape? k)) 
      
      (super-new))))

(define (add-default-keys! k)
  (define (insert-pair in e)
    (send in insert-pair (send e get-key-code)))
  (define (add name key func)
    (send k add-function name func)
    (send k map-function key name))
  (add "paren" "(" insert-pair)
  (add "quote" "\"" insert-pair)
  (add "bracket" "[" insert-pair)
  (add "pipe" "|" insert-pair)
  (add "brace" "{" insert-pair)
  (add "backspace" "backspace" (lambda (in e) (send in do-balanced-delete)))
  (add "del" "del" (lambda (in e) (send in do-balanced-delete))))
