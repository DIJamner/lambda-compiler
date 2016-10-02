#lang racket

(require (for-syntax syntax/parse syntax/stx))

(provide prog)

;; An LLExpr is one of:
;; -Identifier (lambda ID)
;; -Nat (deBruijn index)
;; -(LLExpr LLExpr)

;; A LiftedLambda is a (lambda LLExpr)

;; An LLProg is a (Listof (List Identifier (U LiftedLambda NativeAsm))

(define-syntax (prog stx)
  (syntax-parse stx
    [(prog backend start-exp (name fun) ...)
     #:with main-name (car (generate-temporaries '(main)))
     #:with (main-block ...) (compile-llexp #'start-exp)
     #:with ((code ...) ...) (stx-map compile-llexp #'(fun ...))
     #'(backend main-name
            (main-name set-null-env
                       main-block ...
                       exit)
            (name enter    ;; perform any necessary setup for entering a new function body
                  push-env ;; push this function's argument onto the environment
                  code ... ;; perform the body of the function
                  pop-env  ;; remove this function's argument from the environment
                  return)  ;; return from the function (return value should be in ret-val)
            ...)]))

(define-for-syntax (compile-llexp stx)
    (syntax-parse stx
      [(fun-expr arg-expr)
       #:with (fun-code ...) (compile-llexp #'fun-expr)
       #:with (arg-code ...) (compile-llexp #'arg-expr)
       #'(fun-code ...
          (push ret-val)
          arg-code ...
          (set-arg ret-val)
          (pop ret-val)
          call)]
      [index:nat #'((load (env-get index)))]
      [func:id #'((load-and-bind func (env 0)))]
      [string-lit:str #'((load string-lit))]))