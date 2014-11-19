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
(test "(()" ")" (do-balanced-delete)
      => "()")
(test "((a)" ")" (do-balanced-delete)
      => "((a" "))")
(test "\"" "\"" (do-balanced-delete)
      => "")
;; delete \" from "\""
(test "\"\\\"" "\"" (do-balanced-delete)
      => "\"\"")
;; delete \n from "\n"
(test "\"\\n" "\"" (do-balanced-delete)
      => "\"\"")
;;delete n from "\\n"
(test "\"\\\\n" "\"" (do-balanced-delete)
      => "\"\\\\\"")
;; delete a from "a"
(test "\"a" "\"" (do-balanced-delete)
      => "\"\"") 
(test "\"a\"" "" (do-balanced-delete)
      => "\"a" "\"")
