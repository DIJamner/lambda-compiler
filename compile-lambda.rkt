#lang racket

(require rackunit)

(struct lam (body))

;; an Exp is one of:
;; - (lam Exp)
;; - (list Exp Exp)
;; - Nat

;; an Instr is one of:
;; - '(move addr addr)
;; ...

(define (gen-label)
  (gensym "label"))

(define (gen-string-label)
  (gensym "string"))

(define (compile exp)
  (define-values [res body-max-index]
    (compile-exp exp 'res))
  (if (= body-max-index 0)
      (print-assem (append
                    '(.data
                      (.align 2)
                      not_a_func_err (.asciiz "attempted to call a non-function")
                      .text)
                    res
                    '((li $v0 10)
                      (li $a0 0)
                      syscall
                      ;; builtin print function
                      print
                      (li $v0 4)
                      ;; $a2 should be the address of a string
                      (move $a0 $a2)
                      syscall
                      ;; indicate no return value
                      (la $v0 not_a_func)
                      (move $v1 $zero)
                      (jr $ra)
                      ;; builtin error function
                      not_a_func
                      (li $v0 4)
                      (la $a0 not_a_func_err)
                      syscall
                      (li $v0 10)
                      (li $a0 1)
                      syscall)))
      (error (string-append "Program is not closed. Greatest index is "
                            (number->string body-max-index)))))


;; prints assembly code as a string
(define (print-assem asm)
  (apply string-append
         (map print-instr asm)))

(define/match (print-instr instr)
  [('.text) ".text\n"]
  [('.data) ".data\n"]
  [('syscall) "syscall\n"]
  ;; works for both instructions and directives
  [((cons instr args)) (string-append (symbol->string instr) " "
                                 (print-instr-args args) "\n")]
  [(label) #:when (symbol? label) (string-append (symbol->string label) ":\n")])

(define (token->string tok)
  (cond [(symbol? tok) (symbol->string tok)]
        [(number? tok) (number->string tok)]
        [(string? tok) (string-append "\"" tok "\"")]))

(define (print-instr-args args)
  (apply string-append
         (cons (token->string (first args)) ;; the first arg is always a symbol
               (map (λ (arg)
                      (if (list? arg)
                          (string-append ", "
                                         (number->string (first arg))
                                         "("
                                         (symbol->string (second arg))
                                         ")")
                          (string-append ", "
                                         (token->string arg)))) 
                    (rest args)))))

;; compiles closed expressions
;; produces two values: list of generated code blocks and the highest free index
;;   relative to the top level
;; position: either 'res or 'arg
(define (compile-exp exp position)
  (define-values [codeAddr envAddr]
           (if (equal? position 'res)
               (values '$v0 '$v1) ;; value addresses
               (values '$a1 '$a2))) ;; argument addresses
  (cond [(exact-nonnegative-integer? exp)
         (values (gen-load-var exp codeAddr envAddr '$a0) exp)]
        
        [(lam? exp)
         (define fun-label (gen-label))
         (define end-label (gen-label))
         (define-values [body-code body-max-index]
           (compile-exp (lam-body exp) 'res))
         (define make-env-code
           (if (= body-max-index 0)
               ;; empty environment, use 0
               '((move $s0 $zero))
               '((li $a0 12)
                 (li $v0 9)
                 syscall
                 ;; the syscall sticks the env into v0
                 (sw $a0 (0 $v0))
                 (sw $a1 (4 $v0))
                 (sw $a2 (8 $v0))
                 (sw $s0 (4 $v0))
                 (move $s0 $v0))))
               
         (values (append (gen-prologue fun-label end-label)
                         make-env-code
                         body-code
                         (gen-epilogue end-label)
                         ;; return the environment around this function
                         ;; and its address
                         `((move ,envAddr $s0)
                           (la ,codeAddr ,fun-label)))
                 (max (sub1 body-max-index) 0))]
        [(string? exp)
         (define label (gen-string-label))
         (values `(.data
                   (.align 2)
                   ,label
                   (.asciiz ,exp)
                   .text
                   (la ,codeAddr not_a_func)
                   (la ,envAddr ,label))
                 0)]
        [(list? exp)
         (define fun-exp (first exp))
         (define-values [fun-code fun-max-index]
           (compile-exp fun-exp 'res))
         
         (define arg-exp (second exp))
         (define-values [arg-code arg-max-index]
           (compile-exp arg-exp 'arg))
         (values (append fun-code
                         arg-code
                         '((move $a0 $v1)
                           (sw $ra (8 $sp))
                           (jalr $v0)
                           (lw $ra (8 $sp))))
                 (max fun-max-index arg-max-index))]
        [(equal? exp 'print)
         (values `((la ,codeAddr print)
                   (move ,envAddr $zero))
                 0)]
        [else (error "Invalid input")]))
             

;; generate the function prologue
(define (gen-prologue fun-label end-label)
  `((j ,end-label)
    ,fun-label
    (addi $sp $sp ,(alloc 3))
    (sw $a0 (0 $sp))
    (sw $s0 (4 $sp))))

;; generate the function epilogue
(define (gen-epilogue end-label)
  ;; restore s0's prior value and pop the stack
  `((lw $s0 (4 $sp))
    (addi $sp $sp ,(dealloc 3))
    (jr $ra)
    ,end-label))


;; convenience calculations
(define (alloc n) (* n  -4))
(define (dealloc n) (* n  4))

;; (λλ(1 1) λ0) λ0 is a good test case

;; generate instructions to load the value bound to a deBruijn index
;;  into the value registers given an environment in envReg
(define (gen-load-var var codeAddrTarget envAddrTarget envReg)
  (cond [(= var 0) `((move ,codeAddrTarget $a1)
                     (move ,envAddrTarget $a2))]
        [(= var 1) `((lw ,codeAddrTarget (4 ,envReg))
                     (lw ,envAddrTarget (8 ,envReg)))]
        ;;TODO: choose address intelligently
        [(> var 1) (cons `(lw $t0 (0 ,envReg))
                         (gen-load-var (sub1 var)
                                       codeAddrTarget
                                       envAddrTarget
                                       '$t0))]))

(check-equal? (gen-load-var 0 '$v0 '$v1 '$a0)
              '((move $v0 $a1)
                (move $v1 $a2)))
(check-equal? (gen-load-var 1 '$v0 '$v1 '$a0)
              '((lw $v0 (4 $a0))
                (lw $v1 (8 $a0))))
(check-equal? (gen-load-var 2 '$v0 '$v1 '$a0)
              '((lw $t0 (0 $a0))
                (lw $v0 (4 $t0))
                (lw $v1 (8 $t0))))
(check-equal? (gen-load-var 3 '$v0 '$v1 '$a0)
              '((lw $t0 (0 $a0))
                (lw $t0 (0 $t0))
                (lw $v0 (4 $t0))
                (lw $v1 (8 $t0))))

