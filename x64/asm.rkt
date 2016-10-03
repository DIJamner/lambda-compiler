#lang racket

(require "x64.rkt" (for-syntax syntax/parse syntax/stx racket))

(provide x64)


;; Calling conventions:
;; rax:  1st return reg (code ptr)
;; rdx: 2nd return reg (env ptr)
;; rdi: 1st arg (env)
;; rsi: 2nd arg (arg code ptr)
;; r8: 3rd arg (arg env ptr)
;; rbp: callee-saves (inner env)


(define-syntax (x64 stx)
  (syntax-parse stx
    [(x64 main (name . code) ...)
     #:with ((new-code ...) ...) (stx-map asm->x64 #'((name . code) ...))
     #'(print-x64 (section .text)
         (global _main)
         (extern print)
         (extern new_env)
         (extern not_a_func)
         (_main :)
         (jmp main)
         new-code ... ...)]))

(define-for-syntax (asm->x64 stx)
  (syntax-parse stx
    [(name code ...)
     #:with ((x64-code ...) ...) (stx-map compile-asm-statement #'(code ...))
     #'((name :)
        x64-code ... ...)]))

(define-for-syntax (compile-asm-statement stx)
  (syntax-parse stx #:datum-literals (set-null-env
                                exit
                                push-env pop-env
                                push pop
                                call enter return
                                load
                                arg-val ret-val
                                env
                                env-get)
    [set-null-env #'((mov rbp 0))] ;; rbp contains the inner environment
    [exit #'((mov rax 0x2000001)      ; System call number for exit = 1 on OS X
             (mov rdi 0)              ; Exit failure = 0
             syscall)]                ; Invoke the kernel
    [push-env
     ;; save the outer environment and overwrite it with the inner environment
     #'((add rsp -8)
        (push rbp)
        (call new_env)
        (mov rbp rax))]
    [pop-env
     ;; replace the outer environment
     #'((pop rbp)
        (add rsp 8))]
    [(load arg-val val)
     #:with (codeReg envReg) (get-registers #'val)
     #'((mov rsi codeReg)
        (mov r8 envReg))]
    [(push val)
     #:with (codeReg envReg) (get-registers #'val)
     #'((push codeReg)
        (push envReg))]
    [(pop val)
     #:with (codeReg envReg) (get-registers #'val)
     #'((pop envReg)
        (pop codeReg))]
    [call
     #'((mov rdi rdx) ;;TODO: can I just overwrite rdi like this?
        (call rax))]
    [enter #'((add rsp -8))] ;; align the stack pointer on function entry
    [return #'((add rsp 8) ;; de-align stack pointer before exit
               ret)]
    [(load ret-val (bind fn:id (env n:nat)))
     #:with (follow-links ...) (make-list (syntax->datum #'n) #'(mov rdx [rdx]))
     #'((mov rax fn)
        (mov rdx rbp)
        follow-links ...)]
    [(load ret-val str-lit:str) ;;TODO: parse string for special chars
     #:with label (car (generate-temporaries '(string)))
     #'((section .data)
        (label :)
        (db str-lit 0)
        (section .text)
        (mov rax not_a_func)
        (mov rdx label))]
    [(load ret-val (env-get n:nat))
     #:with (follow-links ...) (make-list (syntax->datum #'n) #'(mov r10 [r10]))
     (if (zero? (syntax->datum #'n))
         #'((mov rax rsi)
            (mov rdx r8))
         #'((mov r10 rbp)
            follow-links ...
            (mov rax [r10 + 8])
            (mov rdx [r10 + 16])))]
    [_ (error "Not a valid statement: " (syntax->datum stx))]))

(define-for-syntax (get-registers stx)
  (syntax-parse stx #:datum-literals (ret-val arg-val)
    [arg-val #'(rsi r8)]
    [ret-val #'(rax rdx)]))