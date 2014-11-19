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
          (delete-from-in-string)]
         [(in-comment?) (delete-one-char)]
         [(after-first-of-empty-pair?) 
          (matching-delete)]
         [(after-last-of-empty-pair?)
          (delete-one-char)
          (delete-one-char)]
         [(after-not-empty-pair?)
          (move-position 'left)]
         [else (delete-one-char)])) 

      ;; ===== private functions =====
      (define/private (delete-from-in-string)
        (cond [(and (at-string-start?)
                    (after-first-of-empty-pair? '((#\" #\"))))
               (matching-delete)]
              [(and (at-string-end?)
                    (after-last-of-empty-pair? '((#\" #\"))))
               (delete-one-char)
               (delete-one-char)]
              [(at-string-end?)
               (move-position 'left)]
              [else (do-escaped-delete)]))
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
        (= start (sub1 (get-start-position))))
      (define/private (at-string-end?)
        (define-values (_start end) (send this get-token-range (get-start-position)))
        (= end (get-start-position)))
      
      (define/private (do-escaped-delete)
        (define pos (- (get-start-position) 2))
        (define l2 (send this get-character pos))
        (cond [(not (eq? l2 #\\))
               (delete-one-char)]
              [(odd? (consecutive-escapes-from pos))
               (delete-one-char)
               (delete-one-char)]
              [else (delete-one-char)]))
      (define/private (consecutive-escapes-from pos)
        (if (not (eq? (send this get-character pos) #\\))
            0
            (add1 (consecutive-escapes-from (sub1 pos)))))

      (define/private (after-first-of-empty-pair? [pairs char-pairs])
        (define pos (get-start-position))
        (define cleft (send this get-character (sub1 pos)))
        (define cright (send this get-character pos))
        (matching-pair? cleft cright pairs))
      (define/private (after-last-of-empty-pair? [pairs char-pairs])
        (define pos (get-start-position))
        (define cright (send this get-character (sub1 pos)))
        (define cleft (send this get-character (- pos 2)))
        (matching-pair? cleft cright pairs))
      
      (define/private (matching-pair? cleft cright pairs)
        (for/or ([pair (in-list pairs)])
          (define-values (left right) (apply values pair))
          (and (eq? left cleft)
               (eq? right cright))))
      (define/private (after-not-empty-pair?)
        (define left (send this get-character (sub1 (get-start-position))))
        (member left (flatten char-pairs)))

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
