#lang racket

(require rackunit)

(require "../asm/lambda.rkt")
(require "test-utils.rkt")

;; test cases
(define assem-0
  (prog ((((lambda (lambda (lambda (2 "test"))))
           (lambda (print 0)))
          (lambda 0))
         (lambda (print 0)))))
(check-equal? (run-asm assem-0) "test")
(define assem-1 (prog (print "test\n")))
(check-equal? (run-asm assem-1) "test\n")