#lang racket

(require (for-syntax syntax/parse)
         (rename-in "mips.rkt"
                    [asm base-asm]))

(provide asm block)

(define-syntax-rule (asm main block ...)
  (base-asm (j main)
            block ...))

(define-syntax (block stx)
  (syntax-parse stx
    [(block name expr ...)
     #:with skip-label (car (generate-temporaries '(skip_label)))
     #'(quasiquote (seq (j skip-label)
                        (name :)
                        expr ...
                        (skip-label :)))]))
