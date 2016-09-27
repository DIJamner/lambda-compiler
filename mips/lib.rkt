#lang racket

(require "block.rkt")

(provide exit const-string return-label not-a-func print-func)

(define-syntax-rule (exit n)
  `(seq (li $v0 10)
        (li $a0 n)
        syscall))

(define-syntax-rule (return-label lbl env)
  `(seq (move $v1 ,env)
        (la $v0 ,lbl)
        (jr $ra)))

(define-syntax-rule (const-string label str)
  `(seq .data
        (.align 2)
        (,label :) (.asciiz ,str)
        .text))

(define-syntax-rule (not-a-func)
  (block not_a_func
         ,(const-string 'not_a_func_err "attempted to call a non-function")
         (li $v0 4)
         (la $a0 not_a_func_err)
         syscall
         ,(exit 1)))

(define-syntax-rule (print-func)
  (block print
         (li $v0 4)
         ;; $a2 should be the address of a string
         (move $a0 $a2)
         syscall
         ;; indicate no return value
         ,(return-label 'not_a_func '$zero)))
