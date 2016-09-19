#lang racket
(require (rename-in "lifted-lambda.rkt"
                   [prog lifted-prog]))

(provide prog)

(define-syntax (prog stx)
  (syntax-case stx ()
    [(prog expr)
     (let-values [((main blocks) (lift-lambdas #'expr))]
     #`(lifted-prog #,main . #,blocks))]))

(define-for-syntax (lift-lambdas expr)
  (syntax-case expr (lambda print)
    [(lambda body)
     (let-values [((body-expr body-blocks) (lift-lambdas #'body))]
       (with-syntax [(lambda-name (car (generate-temporaries '(lambda))))]
         (values #'lambda-name #`((lambda-name #,body-expr) . #,body-blocks))))]
    [(fun arg) ;;TODO: never reached
     (let-values [((fun-expr fun-blocks) (lift-lambdas #'fun))
                  ((arg-expr arg-blocks) (lift-lambdas #'arg))]
       (with-syntax [((arg-block ...) arg-blocks)
                     ((fun-block ...) fun-blocks)]
         (values #`(#,fun-expr #,arg-expr)
                 #'(arg-block ... fun-block ...))))]
    [n (exact-nonnegative-integer? (syntax->datum #'n)) (values #'n #'())]
    [str (string? (syntax->datum #'str)) (values #'str #'())]
    [print (values #'print #'())]))
