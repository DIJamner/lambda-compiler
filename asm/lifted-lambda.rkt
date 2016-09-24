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
     #:with main-block (compile-llexp #'start-exp 'res)
     #:with (fun-block ...) (stx-map (λ (name exp)
                                       #`,(block #,name
                                                 (seq
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
                                                  #,(compile-llexp exp 'res)
                                                  ;; epilogue
                                                  ;; reload the external environment
                                                  (lw $s0 (4 $sp))
                                                  (addi $sp $sp #,(dealloc 3))
                                                  (jr $ra))))
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
  (define (compile-llexp stx position)
    (define-values [codeAddr envAddr]
      (if (equal? position 'res)
          (values '$v0 '$v1)   ;; value addresses
          (values '$a1 '$a2))) ;; argument addresses
    (syntax-parse stx
      [(fun-expr arg-expr)
       #:with fun-code (compile-llexp #'fun-expr 'res)
       #:with arg-code (compile-llexp #'arg-expr 'arg)
       #`(seq fun-code
              arg-code
              (move $a0 $v1)
              (sw $ra (8 $sp))
              (jalr $v0)
              (lw $ra (8 $sp))
              ;; these will sometimes be noops, but later stages can handle optimization
              (move #,codeAddr $v0)
              (move #,envAddr $v1))]
      [index:nat
       (gen-load-var (syntax->datum #'index) codeAddr envAddr '$a0)]
      [func:id
       ;; return the environment around this function
       ;; and its address
       #`(seq (move #,envAddr $s0)
              (la #,codeAddr func))]
      [string-lit:str
       #:with label (car (generate-temporaries '(string)))
       #`(seq .data
              (.align 2)
              (label :)
              (.asciiz ,(syntax->datum #'string-lit))
              .text
              (la #,codeAddr not_a_func)
              (la #,envAddr label))]))


  ;; convenience calculations
  (define (alloc n) (* n  -4))
  (define (dealloc n) (* n  4))

  ;; (λλ(1 1) λ0) λ0 is a good test case
 
  ;; generate instructions to load the value bound to a deBruijn index
  ;;  into the value registers given an environment in envReg
  (define (gen-load-var var codeAddrTarget envAddrTarget envReg)
    (cond [(= var 0) #`(seq (move #,codeAddrTarget $a1)
                            (move #,envAddrTarget $a2))]
          [(= var 1) #`(seq (lw #,codeAddrTarget (4 #,envReg))
                            (lw #,envAddrTarget (8 #,envReg)))]
          ;;TODO: choose address intelligently
          [(> var 1)
             #`(seq (lw $t0 (0 #,envReg))
                    #,(gen-load-var (sub1 var)
                                    codeAddrTarget
                                    envAddrTarget
                                    '$t0))])))
 
