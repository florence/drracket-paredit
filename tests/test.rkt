#lang racket
(require "harness.rkt")
;;; TEST DELETES
(test 
 "(" ")" (do-balanced-delete)
 => "")
(test "((" "))" (do-balanced-delete)
      => "()")
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
;; "\"" -> move cursor left
(test "\"\\\"\"" "" (do-balanced-delete)
      => "\"\\\"" "\"")

;;; TEST INSERT
(test "{" "}" (insert-pair #\")
      => "{\"\"}")
(test "\"" "\"" (insert-pair #\")
      => "\"\\\"\"")
