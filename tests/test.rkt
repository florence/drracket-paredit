#lang racket
(require "harness.rkt")
(test 
 "(" ")" (do-balanced-delete)
 => "")
(test "\"" "\"" (insert-pair #\")
      => "\"\\\"\"")
(test "((" "))" (do-balanced-delete)
      => "()")
(test "{" "}" (insert-pair #\")
      => "{\"\"}")
