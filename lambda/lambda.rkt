#lang racket
(require (for-syntax racket/match syntax/parse)
         (rename-in "debruijn-lambda.rkt"
                    [prog debruijn-prog]))

(provide prog)

(define-syntax (prog stx)
  (syntax-parse stx
    [(prog backend expr)
     #:with debruijn-expr (convert-debruijn-indices #'expr empty-ids)
     #`(debruijn-prog backend debruijn-expr)]))

(begin-for-syntax
  ;; An IdEnv is one of:
  ;; - empty-ids
  ;; - (extend-ids Id IdEnv)
  (define empty-ids '())
  (define extend-ids cons)

  ;; find-index : IdEnv Id Natural -> Natural
  (define (find-index ids x i)
    (match ids
      ['() (raise-syntax-error #f "unbound identifier" x)]
      [(cons id ids)
       (if (bound-identifier=? id x)
           i
           (find-index ids x (add1 i)))]))
  
  ;; convert-debruijn-indices : Stx IdEnv -> Stx
  (define (convert-debruijn-indices stx ids)
    (syntax-parse stx #:literals (lambda print)
      [print #'print]
      [x:id (find-index ids #'x 0)]
      [(lambda (x:id) body:expr)
       #:with debruijn-body (convert-debruijn-indices #'body (extend-ids #'x ids))
       #'(lambda debruijn-body)]
      [(fun:expr arg:expr)
       #:with debruijn-fun (convert-debruijn-indices #'fun ids)
       #:with debruijn-arg (convert-debruijn-indices #'arg ids)
       #'(debruijn-fun debruijn-arg)]
      [n:nat #'n]
      [str:str #'str]
      )))
