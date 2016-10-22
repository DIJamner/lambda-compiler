#lang racket

(require (for-syntax syntax/parse syntax/stx syntax/free-vars syntax/id-set
                     graph racket))

(provide prog unify-functions)

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
;; callstack[(Listof IRCont)]
;; stdout[String]

;; An IRProg is a (Backend Identifier IRFunc ...)

;; An IRFunc is a (Identifier IRStmnt ...)

;; An IRCont is a (Listof IRStmnt)
;;   An IRCont represents a continuation

;; An IRStmnt is one of:
;; - set-null-env: sets the inner environment to the null pointer
;; - exit: terminates the program
;; - enter: perform any necessary setup after entering a function
;; - push-env: push arg-val onto the environment
;; - pop-env: remove the top element of the environment
;; - return: pop the top of the callstack and execute it
;; - call: push the rest of the code in this function on the callstack and execute the code at ret-val
;; - (push IRVar): add the value in IRVar to the top of the stack
;; - (pop IRVar): set IRVar to be the top value of the stack and remove it from the stack 
;; - (load IRVar IRExp): set IRVar to IRVal
;;
;; When an IRStmnt is executed, it increments the program-counter

;; An IRVar is one of:
;; - ret-val
;; - arg-val

;; An IRVal is one of:
;; - (bind Identifier Env): make a closure with the code at Identifier and the environment Env
;; - String: a String literal

;; An IRExp is one of:
;; - IRVal
;; - (env-get Nat): gets the IRVal Nat levels into the inner environment
;; - IRVar: gets the value stored in the IRVar

;; An Env is a (env Nat)
;;   An Env represents the environment that closes over all arguments
;;     with deBruijn index >= Nat and subtracts Nat from their indices

(define-syntax (prog stx)
  (syntax-parse stx
    [(prog backend #:lifted-opt (opt opt-rest ...) start-exp (name fun) ...)
     #`(opt backend #:lifted-opt (opt-rest ...) start-exp (name fun) ...)]
    [(prog backend (~optional (~seq #:lifted-opt ())) start-exp (name fun) ...)
     #:with main-name (car (generate-temporaries '(main)))
     #:with (main-block ...) (compile-llexp #'start-exp)
     #:with ((code ...) ...) (stx-map compile-llexp #'(fun ...))
     #'(backend main-name
            (main-name set-null-env
                       main-block ...
                       exit)
            (name (enter name) ;; perform any necessary setup for entering a new function body
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


;; peforms an exhaustive search to unify functions with identical bodies
;; and remove dead code
;; TODO: does not handle recursive functions
(define-syntax (unify-functions stx)
  (syntax-parse stx
    [(unify-functions backend #:lifted-opt (opt-rest ...) start-exp (name fun) ...)
     #:do [(define-values (used-by uses) (used-funs #'start-exp #'((name fun) ...)))
           (define simplify-list (dedup-ordered-list uses))
           (define new-function-map (foldl simplify-if-possible
                                           (fun-hash #'((name fun) ...)) simplify-list))]
     #:with ((uname ufun) ...) (map (lambda (p) (list (car p) (cdr p)))
                                    (hash->list new-function-map))
     #'(prog backend #:lifted-opt (opt-rest ...) start-exp (uname ufun) ...)]))

(define-for-syntax (simplify-if-possible fun fun-map)
  (define equal-fun (find-equal-function fun-map fun))
  (if equal-fun
      (replace-function fun-map equal-fun fun)
      fun-map))

;; replace one function with an existing identical one
(define-for-syntax (replace-function fun-map to-use to-remove)
  (for/hash ([(k v) (in-hash fun-map)]
             #:unless (equal? k to-remove))
    (values k (subst v to-use to-remove))))

;; replace a variable with an expr in a lifted lambda expression
(define-for-syntax (subst exp replace-exp var)
  (syntax-parse exp
    [(fun arg)
     #:with sub-fun (subst #'fun replace-exp var)
     #:with sub-arg (subst #'arg replace-exp var)
     #'(sub-fun sub-arg)]
    [n:nat #'n]
    [s:str #'s]
    [v:id #:when (equal? var #'v)
          replace-exp]
    [v:id #'v]))

;; find a syntactically equal function
(define-for-syntax (find-equal-function fun-map fun)
  (define fun-body (hash-ref fun-map fun))
  (foldl (lambda (k res)
           (if (and (not res) (equal? fun-body (hash-ref k)))
               k
               res))
         #f
         (hash-keys fun-map)))

;; computes an ordered list of function identifiers such that deduplicating them
;; in the given order will give the best result
(define-for-syntax (dedup-ordered-list uses)
  (cond [(hash-empty? uses) (values (list) uses)]
        [else
         (define assoc-list (hash->list uses))
         (define graph (unweighted-graph/adj assoc-list))
         (define-values (discover pred finish) (dfs graph))
         (define vertex-list (hash-keys uses))
         (sort vertex-list
               #:key (lambda (v) (hash-ref finish v))
               <)])) ;;TODO: < or >??????!
  
  

;; compute the ε-closure of the input
;; TODO: fails to account for loops (loops do no curr. occur, but may in the future)
(define-for-syntax (used-funs exp funs)
  (define worklist (get-free-vars exp))
  (define function-map (fun-hash funs))
  (define uses-map (var-hash function-map))
  (define used-by-map (empty-var-hash function-map))
  (used-funs-closure used-by-map uses-map worklist))

(define-for-syntax (used-funs-closure used-by-map uses-map worklist)
  (cond [(empty? worklist) (values used-by-map uses-map)]
        [else
         ;; get an element from the worklist to update
         (define e (first worklist))
         ;; get the currently known functions it uses
         (define e-uses (hash-ref uses-map e))
         ;; get the currently known functions that use it
         (define e-used-by (hash-ref used-by-map e))
         ;; get new knowledge of which functions e uses
         (define updated-e-uses
           (apply free-id-set-union
                   (free-id-set-map e-uses (lambda (e-use) (hash-ref uses-map e-use)))))
         ;; find which ones are new
         (define new-e-uses
           (free-id-set-subtract
            updated-e-uses
            e-uses))
         ;; if we got no new knowledge, we are done with e
         ;; otherwise, update the uses map and used-by map
         (if (zero? (free-id-set-count new-e-uses))
             (used-funs-closure used-by-map uses-map (rest worklist))
             (used-funs-closure (add-use used-by-map new-e-uses e)
                                (hash-set uses-map e updated-e-uses)
                                worklist))]))

(define-for-syntax (add-use used-by-map new-e-uses e)
  (foldl (lambda (map x)
           (hash-update map x (lambda (s) (free-id-set-add s e))))
         used-by-map
         (free-id-set->list new-e-uses)))


(define-for-syntax (empty-var-hash fun-hash)
  (define keys (hash-keys fun-hash))
  (foldr (lambda (x rst) (cons x (cons '() rst))) keys (list)))
  

(define-for-syntax (var-hash fun-hash)
  (define keys (hash-keys fun-hash))
  (define key/val-pairs (map (lambda (k) (list k (get-free-vars (hash-ref fun-hash k)))) keys))
  (apply hash (list->set (apply append key/val-pairs))))

(define-for-syntax (fun-hash stx)
  (apply hash (apply append (map syntax->list (syntax->list stx)))))

(define-for-syntax (get-free-vars stx)
  (parameterize ([current-namespace (make-empty-namespace)])
    (free-vars (expand stx))))