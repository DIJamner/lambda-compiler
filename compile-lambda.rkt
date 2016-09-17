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

(define (label-generator x)
  (λ () (values (string->symbol (string-append "label"
                                               (number->string x)))
          (label-generator (add1 x)))))
;;tests

(define test-gen-0 (label-generator 0))
(define-values [test-label-1 test-gen-1] (test-gen-0))
(define-values [test-gen-l test-gen-r] (split-generator test-gen-1))
(define-values [test-label-l test-next-gen-l] (test-gen-l))
(define-values [test-label-r test-next-gen-r] (test-gen-r))
(define-values [test-label-l-2 test-next-gen-l-2] (test-next-gen-l))
(define-values [test-label-r-2 test-next-gen-r-2] (test-next-gen-r))

(check-not-equal? test-label-l test-label-r)
(check-not-equal? test-label-1 test-label-l)
(check-not-equal? test-label-1 test-label-r)
(check-not-equal? test-label-l-2 test-label-r)
(check-not-equal? test-label-l-2 test-label-l)
(check-not-equal? test-label-r-2 test-label-r)
(check-not-equal? test-label-r-2 test-label-l)



;; end tests

(define (compile exp)
  (define-values [res body-max-index]
    (compile-exp (label-generator 0) exp 'res))
  (if (= body-max-index 0)
      (print-assem (append
                    '(.text)
                    res
                    '((li $v0 10)
                      (li $a0 0)
                      syscall)))
      (error (string-append "Program is not closed. Greatest index is "
                            (number->string body-max-index)))))


;; prints assembly code as a string
(define (print-assem asm)
  (apply string-append
         (map print-instr asm)))

(define/match (print-instr instr)
  [('.text) ".text\n"]
  [('syscall) "syscall\n"]
  [((cons instr args)) (string-append (symbol->string instr) " "
                                 (print-instr-args args) "\n")]
  [(label) #:when (symbol? label) (string-append (symbol->string label) ":\n")])

(define (print-instr-args args)
  (apply string-append
         (cons (symbol->string (first args)) ;; the first arg is always a symbol
               (map (λ (arg)
                      (if (list? arg)
                          (string-append ", "
                                         (number->string (first arg))
                                         "("
                                         (symbol->string (second arg))
                                         ")")
                          (string-append ", "
                                         (if (symbol? arg)
                                             (symbol->string arg)
                                             (number->string arg))))) 
                    (rest args)))))

;; compiles closed expressions
;; produces two values: list of generated code blocks and the highest free index
;;   relative to the top level
;; position: either 'res or 'arg
(define (compile-exp label-gen exp position)
  (define-values [codeAddr envAddr]
           (if (equal? position 'res)
               (values '$v0 '$v1) ;; value addresses
               (values '$a1 '$a2))) ;; argument addresses
  (cond [(exact-nonnegative-integer? exp)
         (values (gen-load-var exp codeAddr envAddr '$a0) exp)]
        
        [(lam? exp)
         (define-values [fun-label next-gen-1] (label-gen))
         (define-values [end-label next-gen-2] (next-gen-1))
         (define-values [body-code body-max-index]
           (compile-exp next-gen-2 (lam-body exp) 'res))
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
             
        [(list? exp)
         (define-values [gen-l gen-r] (split-generator label-gen))
         
         (define fun-exp (first exp))
         (define-values [fun-code fun-max-index]
           (compile-exp gen-l fun-exp 'res))
         
         (define arg-exp (second exp))
         (define-values [arg-code arg-max-index]
           (compile-exp gen-r arg-exp 'arg))
         (values (append fun-code
                         arg-code
                         '((move $a0 $v1)
                           (jalr $v0)))
                 (max fun-max-index arg-max-index))]
         
        [else (error "Invalid input")]))
             

;; generate the function prologue
(define (gen-prologue fun-label end-label)
  `((j ,end-label)
    ,fun-label
    (addi $sp $sp -8)
    (sw $a0 (0 $sp))
    (sw $s0 (4 $sp))))

;; generate the function epilogue
(define (gen-epilogue end-label)
  ;; restore s0's prior value and pop the stack
  `((lw $s0 (4 $sp))
    (addi $sp $sp 8)
    (jr $ra)
    ,end-label))

;; (λλ(1 1) λ0) λ0 is a goood test case

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

