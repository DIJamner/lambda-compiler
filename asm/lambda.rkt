#lang racket
(require (for-syntax syntax/parse)
         (rename-in "lifted-lambda.rkt"
                   [prog lifted-prog]))

(provide prog)

(define-syntax (prog stx)
  (syntax-parse stx
    [(prog expr)
     (define-values [main blocks] (lift-lambdas #'expr))
     #`(lifted-prog #,main . #,blocks)]))

(define-for-syntax (lift-lambdas expr)
  (syntax-parse expr #:literals (lambda print)
    [(lambda body)
     #:do [(define-values [body-expr body-blocks] (lift-lambdas #'body))]
     #:with lambda-name (car (generate-temporaries '(lambda)))
     (values #'lambda-name #`((lambda-name #,body-expr) . #,body-blocks))]
    [(fun arg) ;;TODO: never reached
     #:do [(define-values [fun-expr fun-blocks] (lift-lambdas #'fun))
           (define-values [arg-expr arg-blocks] (lift-lambdas #'arg))]
     #:with (arg-block ...) arg-blocks
     #:with (fun-block ...) fun-blocks
     (values #`(#,fun-expr #,arg-expr)
             #'(arg-block ... fun-block ...))]
    [n:nat (values #'n #'())]
    [str:str (values #'str #'())]
    [print (values #'print #'())]))
