#lang racket

(require rackunit)

(require "../asm/lambda.rkt")
(require "test-utils.rkt")

;; test cases
(test-case "test stuff"
  (define assem
    (prog ((((lambda (lambda (lambda (2 "test"))))
             (lambda (print 0)))
            (lambda 0))
           (lambda (print 0)))))
  (check-equal? (run-asm assem) "test"))

(test-case "(print \"test\\n\")"
  (define assem (prog (print "test\n")))
  (check-equal? (run-asm assem) "test\n"))

(test-case "(print (((λλ(1 1) λ0) λ0) \"a string\"))"
  ;; (((lambda (lambda (1 1))) (lambda 0)) (lambda 0))
  ;; should be an identity function
  (define assem
    (prog (print ((((lambda (lambda (1 1))) (lambda 0)) (lambda 0)) "a string"))))
  (check-equal? (run-asm assem) "a string"))

(test-case "not a function"
  (define assem (prog ("test" (lambda 0))))
  (check-equal? (run-asm assem) "attempted to call a non-function"))
