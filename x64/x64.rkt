#lang racket

(require (for-syntax syntax/parse syntax/stx racket))

(provide print-x64)

(define-syntax (print-x64 stx)
  (syntax-parse stx
    [(print-x64 . asm)
     (datum->syntax stx (string-join (stx-map print-statement #'asm) "\n"))]))

(define-for-syntax (print-statement stmt)
  (define-syntax-class oper
    #:description "NASM x64 operation"
    #:datum-literals (add push pop call ret mov jmp)
    (pattern (~or add push pop call ret mov jmp)))
  (define-syntax-class reg
    #:description "NASM x64 register"
    #:datum-literals (rax rbx rcx rdx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)
    (pattern (~or rax rbx rcx rdx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15)))
  (define-syntax-class arg
    #:description "argument to a NASM x64 operation"
    #:literals (+)
    (pattern (~or r:reg n:integer lbl:id [a:arg] [a:arg + b:arg])))
  (define-syntax-class stmnt
    #:description "NASM x64 statement"
    (pattern (o:oper a:arg ...)))
  (syntax-parse stmt #:datum-literals (section .text .data
                                       extern global : db
                                       syscall ret)
    [syscall "syscall"]
    [ret "ret"]
    [(section .text) "section .text"]
    [(section .data) "section .data"]
    [(db str-lit:str 0) (string-append "db \"" (syntax->datum #'str-lit) "\", 0")]
    [(extern lbl:id) (string-append "extern "
                      (symbol->string
                       (syntax->datum #'lbl)))]
    [(global lbl:id) (string-append "global "
                      (symbol->string
                       (syntax->datum #'lbl)))]
    [(lbl:id :) (string-append
                 (symbol->string
                  (syntax->datum #'lbl)) ":")]
    [s:stmnt (string-join (stx-map print-arg #'(s.a ...))
                                 ", "
                                 #:before-first (string-append
                                                 (symbol->string
                                                  (syntax->datum #'s.o)) " "))]
    [_ (error "statement could not be printed")]))

(define-for-syntax (print-arg arg)
  (syntax-parse arg #:literals (+)
    [lbl:id (symbol->string (syntax->datum #'lbl))]
    [n:integer (number->string (syntax->datum #'n))]
    [(a) (string-append "[" (print-arg #'a) "]")]
    [(r + n) (string-append "[" (print-arg #'r) " + " (print-arg #'n) "]")]
    [_ (error "argument could not be printed")]))
    