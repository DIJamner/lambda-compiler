#lang racket

(require rackunit)

(require "../lambda/lambda.rkt")
(require "../mips/asm.rkt")
(require "test-utils.rkt")

;; test cases
(test-case "test stuff"
  (define assem
    (prog mips ((((lambda (x) (lambda (y) (lambda (z) (x "test"))))
                  (lambda (x) (print x)))
                 (lambda (x) x))
                (lambda (x) (print x)))))
  (check-equal? (run-mips assem) "test"))

(test-case "(print \"test\\n\")"
  (define assem (prog mips (print "test\n")))
  (check-equal? (run-mips assem) "test\n"))

(test-case "(print (((λλ(1 1) λ0) λ0) \"a string\"))"
  ;; (((lambda (lambda (1 1))) (lambda 0)) (lambda 0))
  ;; should be an identity function
  (define assem
    (prog mips (print ((((lambda (x) (lambda (y) (x x))) (lambda (x) x)) (lambda (x) x)) "a string"))))
  (check-equal? (run-mips assem) "a string"))

(test-case "not a function"
  (define assem (prog mips ("test" (lambda (x) x))))
  (check-equal? (run-mips assem) "attempted to call a non-function"))
