#lang racket

(require "block.rkt" "lib.rkt"
         (for-syntax syntax/parse syntax/stx))

(provide prog)

;; An LLExpr is one of:
;; -Identifier (lambda ID)
;; -Nat (deBruijn index)
;; -(LLExpr LLExpr)

;; A LiftedLambda is a (lambda LLExpr)

;; An LLProg is a (Listof (List Identifier (U LiftedLambda NativeAsm))

(define-syntax (prog stx)
  (syntax-parse stx
    [(prog start-exp (name fun) ...)
     #:with main-name (car (generate-temporaries '(main)))
     #:with main-block (compile-llexp #'start-exp)
     #:with (fun-block ...) (stx-map (λ (name exp)
                                       #`,(block #,name
                                                 (addi $sp $sp #,(alloc 3))
                                                 (sw $a0 (0 $sp))
                                                 (sw $s0 (4 $sp))
                                                 ;;allocate environment on the heap
                                                 (li $a0 12)
                                                 (li $v0 9)
                                                 syscall
                                                 (lw $a0 (0 $sp))
                                                 ;; store environment contents
                                                 (sw $a0 (0 $v0))
                                                 (sw $a1 (4 $v0))
                                                 (sw $a2 (8 $v0))
                                                 ;; inner environment is stored in s0
                                                 (move $s0 $v0)
                                                 #,(compile-llexp exp)
                                                 ;; epilogue
                                                 ;; reload the external environment
                                                 (lw $s0 (4 $sp))
                                                 (addi $sp $sp #,(dealloc 3))
                                                 (jr $ra)))
                                     #'(name ...)
                                     #'(fun ...))
     #'(asm main-name
            ,(block main-name
                    main-block
                    ,(exit 0))
            ,(not-a-func)
            ,(print-func)
            fun-block ...)]))

(begin-for-syntax
  (define (compile-llexp stx)
    (syntax-parse stx
      [(fun-expr arg-expr)
       #:with fun-code (compile-llexp #'fun-expr)
       #:with arg-code (compile-llexp #'arg-expr)
       #`(seq (addi $sp $sp #,(alloc 3))
              fun-code
              (sw $v0 (0 $sp))
              (sw $v1 (4 $sp))
              arg-code
              (move $a1 $v0)
              (move $a2 $v1)
              (lw $v0 (0 $sp))
              (lw $v1 (4 $sp))
              (move $a0 $v1)
              (sw $ra (8 $sp))
              (jalr $v0)
              (lw $ra (8 $sp))
              (addi $sp $sp #,(dealloc 3)))]
      [index:nat
       (gen-load-var (syntax->datum #'index) '$a0)]
      [func:id
       ;; return the environment around this function
       ;; and its address
       #`(seq (la $v0 func)
              (move $v1 $s0))]
      [string-lit:str
       #:with label (car (generate-temporaries '(string)))
       #`(seq .data
              (.align 2)
              (label :)
              (.asciiz ,(syntax->datum #'string-lit))
              .text
              (la $v0 not_a_func)
              (la $v1 label))]))

  ;; convenience calculations
  (define (alloc n) (* n  -4))
  (define (dealloc n) (* n  4))
 
  ;; generate instructions to load the value bound to a deBruijn index
  ;;  into the value registers given an environment in envReg
  (define (gen-load-var var envReg)
    (cond [(= var 0) #`(seq (move $v0 $a1)
                            (move $v1 $a2))]
          [(= var 1) #`(seq (lw $v0 (4 #,envReg))
                            (lw $v1 (8 #,envReg)))]
          ;;TODO: choose address intelligently
          [(> var 1)
             #`(seq (lw $t0 (0 #,envReg))
                    #,(gen-load-var (sub1 var) '$t0))])))
 
