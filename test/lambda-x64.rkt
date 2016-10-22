#lang racket

(require "../lambda/lambda.rkt" "../x64/asm.rkt" "test-utils.rkt")


;;TODO: ((lambda (0 0)) (lambda (0 0)) causes segfault
;;TODO: ((lambda (0 0)) (lambda (((lambda 0) 0) 0)) runs out of memory (should call collect and exit?)
;;        -calls collect and exits when there is a breakpoint at collect
(define assem
  (prog x64 #:lifted-opt (unify-functions)
        ((lambda ((lambda 0) 0))
         ((lambda ((lambda 0) 0))
          ((lambda ((lambda 0) 0))
           ((lambda ((lambda 0) 0))
            ((lambda ((lambda 0) 0))
             ((lambda ((lambda 0) 0))
              ((lambda ((lambda 0) 0))
               ((lambda ((lambda 0) 0))
                ((lambda ((lambda 0) 0))
                 ((lambda ((lambda 0) 0))
                  ((lambda ((lambda 0) 0))
                   ((lambda ((lambda 0) 0))
                    ((lambda ((lambda 0) 0))
                     ((lambda ((lambda 0) 0))
                      ((lambda ((lambda 0) 0))
                       ((lambda ((lambda 0) 0))
                        ((lambda ((lambda 0) 0))
                         ((lambda ((lambda 0) 0))
                          ((lambda ((lambda 0) 0)) "test")))))))))))))))))))))
(define assem2 (prog x64 ((print "test") "test2")))
(display assem)
;(run-x64 assem)