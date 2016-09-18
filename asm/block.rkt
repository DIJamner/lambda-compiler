#lang racket

(require (rename-in "../asm.rkt"
                    [asm base-asm]))

(provide asm block)

(define-syntax-rule (asm main block ...)
  (base-asm (j main)
            block ...))

(define-syntax (block stx)
  (syntax-case stx ()
    [(block name expr ...)
     (with-syntax [(skip-label (car (generate-temporaries '(skip_label))))]
       #'(quasiquote (seq (j skip-label)
                          (name :)
                          expr ...
                          (skip-label :))))]))
