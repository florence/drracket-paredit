#lang racket
(module+ main
  (require framework racket/gui)
  (require paredit/private/paredit)
  (define f
    (new frame:basic% [label "frame"]
         [width 600]
         [height 700]))
  (define t
    (new (paredit-text-mixin racket:text%)))
  (define ec
    (new editor-canvas%
         [parent (send f get-area-container)]
         [editor t]))

  ;;(send t load-file "kshfek")
  (send t set-filename #f)

  (define k (new keymap%))

  (send t set-keymap k)
  (add-default-keys! k)

  (send ec focus)
  (send f show #t))
