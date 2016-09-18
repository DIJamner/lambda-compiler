#lang racket

(provide (except-out (all-from-out racket)
                     #%module-begin)
         (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin expr ...)
  (#%module-begin
   (define assem (print-assem '(expr ...)))
   (display assem)
   (provide assem)))

;; prints assembly code as a string
(define (print-assem asm)
  (apply string-append
         (map print-instr asm)))

(define/match (print-instr instr)
  [('.text) ".text\n"]
  [('.data) ".data\n"]
  [('syscall) "syscall\n"]
  [((list label ': )) #:when (symbol? label) (string-append (symbol->string label) ":\n")]
  ;; works for both instructions and directives
  [((cons instr args)) (string-append (symbol->string instr) " "
                                 (print-instr-args args) "\n")]
  [(_) (error "Instruction or directive not supported.")])

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