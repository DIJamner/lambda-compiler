#lang racket

(provide exit const-string return)

(define-syntax-rule (exit n)
  `(seq (li $v0 10)
        (li $a0 n)
        syscall))

(define-syntax-rule (return) '(jr $ra))

(define-syntax-rule (const-string label str)
  `(seq  .data
         (.align 2)
         (,label :) (.asciiz ,str)
         .text))