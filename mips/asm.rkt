#lang racket

(require "mips.rkt" (for-syntax syntax/parse syntax/stx racket))

(provide asm)

(define-syntax (asm stx)
  (syntax-parse stx
    [(asm main (name . code) ...)
     #:with ((new-code ...) ...) (stx-map asm->mips #'((name . code) ...))
     #'(print-mips '(.text
                      (j main)
                      new-code ... ...
                      ;; library functions
                      (not_a_func :)
                      .data
                      (.align 2)
                      (not_a_func_err :)
                      (.asciiz "attempted to call a non-function")
                      .text
                      (li $v0 4)
                      (la $a0 not_a_func_err)
                      syscall
                      (li $v0 10)
                      (li $a0 1)
                      syscall
                      (print :)
                      (li $v0 4)
                      (move $a0 $a2)
                      syscall
                      (move $v1 $zero)
                      (la $v0 not_a_func)
                      (jr $ra)))]))

(define-for-syntax (asm->mips stx)
  (syntax-parse stx
    [(name code ...)
     #:with ((mips-code ...) ...) (stx-map compile-asm-statement #'(code ...))
     #'((name :)
        mips-code ... ...)]))

(define-for-syntax (compile-asm-statement stx)
  (syntax-parse stx #:datum-literals (set-null-env
                                exit
                                push-env pop-env
                                set-arg
                                push pop
                                call return
                                load-and-bind
                                load
                                env
                                env-get)
    [set-null-env #'((move $s0 $zero))]
    [exit #'((li $v0 10)
             (li $a0 0)
             syscall)]
    [(push-env val)
     #:with (codeReg envReg) (get-registers #'val)
     #'((addi $sp $sp 8) ;; allocate 2 spaces:
        (sw $a0 (0 $sp))  ;; one space for the environment
        (sw $s0 (4 $sp))  ;; one space for $s0 since it's callee-saves
        ;; allocate a new environment on the heap
        (li $a0 12)
        (li $v0 9)
        syscall
        ;; reload the outer environment
        (lw $a0 (0 $sp))
        ;; store inner environment contents
        (sw $a0 (0 $v0))
        (sw codeReg (4 $v0))
        (sw envReg (8 $v0))
        ;; inner environment is stored in s0
        (move $s0 $v0))]
    [pop-env
     #'((lw $s0 (4 $sp));; reload the external environment
        (addi $sp $sp -8))]
    [(set-arg val)
     #:with (codeReg envReg) (get-registers #'val)
     #'((move $a1 codeReg)
        (move $a2 envReg))]
    [(push val)
     #:with (codeReg envReg) (get-registers #'val)
     #'((addi $sp $sp 8)
        (sw codeReg (0 $sp))
        (sw envReg (4 $sp)))]
    [(pop val)
     #:with (codeReg envReg) (get-registers #'val)
     #'((lw codeReg (0 $sp))
        (lw envReg (4 $sp))
        (addi $sp $sp -8))]
    [call
     #'((addi $sp $sp 4)
        (sw $ra (0 $sp))
        (move $a0 $v1)
        (jalr $v0)
        (lw $ra (0 $sp))
        (addi $sp $sp -4))]
    [return #'((jr $ra))]
    [(load-and-bind fn:id (env n:nat))
     #:with (follow-links ...) (make-list (syntax->datum #'n) #'(lw $v1 (0 $v1)))
     #'((la $v0 fn)
        (move $v1 $s0)
        follow-links ...)]
    [(load str-lit:str)
     #:with label (car (generate-temporaries '(string)))
     #'(.data
        (.align 2)
        (label :)
        (.asciiz str-lit)
        .text
        (la $v0 not_a_func)
        (la $v1 label))]
    [(load (env-get n:nat))
     #:with (follow-links ...) (make-list (syntax->datum #'n) #'(lw $t1 (0 $t1)))
     (if (zero? (syntax->datum #'n))
         #'((move $v0 $a1)
            (move $v1 $a2))
         #'((move $t1 $s0)
            follow-links ...
            (lw $v0 (4 $t1))
            (lw $v1 (8 $t1))))]
    [_ (error "Not a valid statement: " (syntax->datum stx))]))

(define-for-syntax (get-registers stx)
  (syntax-parse stx #:datum-literals (ret-val arg-val)
    [arg-val #'($a1 $a2)]
    [ret-val #'($v0 $v1)]))