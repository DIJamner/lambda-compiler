#lang racket

(provide asm)

(define-syntax-rule (asm . exprs)
  (begin
    (define assem (print-assem `exprs))
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
  [((cons 'seq instrs)) (print-assem instrs)]
  [('no-instr) ""]
  [((list label ': )) #:when (symbol? label) (string-append (symbol->string label) ":\n")]
  ;; works for both instructions and directives
  [((cons instr args)) (string-append (symbol->string instr) " "
                                 (print-instr-args args) "\n")]
  [(_) (error "Instruction or directive not supported: " instr)])

(define (token->string tok)
  (cond [(symbol? tok) (symbol->string tok)]
        [(number? tok) (number->string tok)]
        [(string? tok) (string-append "\"" tok "\"")]))

(define (token? arg)
  (or (symbol? arg) (number? arg) (string? arg)))

(define (print-instr-args args)
  (string-join (map (λ (arg)
                      (match arg
                        [(list n reg) #:when (and (number? n) (symbol? reg))
                                      (string-append ", "
                                                     (number->string (first arg))
                                                     "("
                                                     (symbol->string (second arg))
                                                     ")")]
                        [_ #:when (token? arg)
                           (string-append " " (token->string arg))]
                        [_ (error "Failed to match: " arg)]))
                    (rest args))
               " " ;;TODO: fix commas
               #:before-first (token->string (first args))))


