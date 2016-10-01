#lang racket

(require (for-syntax syntax/parse syntax/stx racket))

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
     #''((section .text)
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
        (add rsp -8) ;; align the stack pointer. "return" handles the inverse.
        x64-code ... ...)]))

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
    [set-null-env #'((mov rbp 0))] ;; rbp contains the inner environment
    [exit #'((mov rax 0x2000000)      ; System call number for exit = 0 on OS X
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
    [(set-arg val)
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
    [return #'(ret)]
    [(load-and-bind fn:id (env n:nat))
     #:with (follow-links ...) (make-list (syntax->datum #'n) #'(mov rdx [rdx]))
     #'((mov rax fn)
        (mov rdx rdp)
        follow-links ...)]
    [(load str-lit:str)
     #:with label (car (generate-temporaries '(string)))
     #'((section .data)
        (label :)
        (db "Error: tried to call a non-function" 0)
        (section .text)
        (mov rax not_a_func)
        (mov rdx label))]
    [(load (env-get n:nat))
     #:with (follow-links ...) (make-list (syntax->datum #'n) #'(lw r10 (0 r10)))
     (if (zero? (syntax->datum #'n))
         #'((mov rax rsi)
            (mov rdx r8))
         #'((mov r10 rdp)
            follow-links ...
            (mov rax [r10 + 8])
            (mov rdx [r10 + 16])))]
    [_ (error "Not a valid statement: " (syntax->datum stx))]))

(define-for-syntax (get-registers stx)
  (syntax-parse stx #:datum-literals (ret-val arg-val)
    [arg-val #'(rsi r8)]
    [ret-val #'(rax rdx)]))