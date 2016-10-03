#lang racket

(require (for-syntax syntax/parse syntax/stx))

(provide prog)

;; An LLExpr is one of:
;; -Identifier (lambda ID)
;; -Nat (deBruijn index)
;; -(LLExpr LLExpr)

;; A LiftedLambda is a (lambda LLExpr)

;; An LLProg is a (List Identifier (Listof (List Identifier LiftedLambda))


;; IR Computational Model:
;; Format: <state-elem>[<contents>] means that the state component <state-elem>
;;   contains a value of the form <contents>
;;
;; State:
;; ret-val[IRVal]
;; arg-val[IRVal]
;; env[(Listof IRVal)]
;; stack[(Listof IRVal)]
;; program-counter[Nat]
;; callstack[(Listof (List Identifier Nat))]
;; stdout[String]

;; An IRProg is a (Backend Identifier IRFunc ...)

;; An IRFunc is a (Identifier IRStmnt ...)

;; An IRStmnt is one of:
;; - set-null-env: sets the inner environment to the null pointer
;; - exit: terminates the program
;; - enter: perform any necessary operations after entering a function
;; - push-env: push arg-val onto the environment
;; - pop-env: remove the top element of the environment
;; - return: pop the current function off of the call stack, set the program counter to the return address
;;             and resume the function at the top of the stack
;; - call: push ret-val and the address of the next statement on the callstack and execute the code at ret-val
;; - (push IRVar): add the value in IRVar to the top of the stack
;; - (pop IRVar): set IRVar to be the top value of the stack and remove it from the stack 
;; - (load IRVar IRVal): set IRVar to IRVal
;;
;; When an IRStmnt is executed, it increments the program-counter

;; An IRVar is one of:
;; - ret-val
;; - arg-val

;; An IRVal is one of:
;; - (bind Identifier Env): make a closure with the code at Identifier and the environment Env
;; - String: a String literal
;; - (env-get Nat): gets the IRVal Nat levels into the inner environment
;; - IRVar: gets the value stored in the IRVar

;; An Env is a (env Nat)
;;   An Env represents the environment that closes over all arguments
;;     with deBruijn index >= Nat and subtracts Nat from their indices

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
          (load arg-val ret-val)
          (pop ret-val)
          call)]
      [index:nat #'((load ret-val (env-get index)))]
      [func:id #'((load ret-val (bind func (env 0))))]
      [string-lit:str #'((load ret-val string-lit))]))