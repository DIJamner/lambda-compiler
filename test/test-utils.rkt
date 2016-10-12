#lang racket

(require racket/system racket/runtime-path)

(provide run-mips run-x64)


(define-runtime-path mars "mars/Mars4_5.jar")

(define-runtime-path build "../x64/lib/build.sh")

;; run-mips : MIPSString -> String
(define (run-mips assembly)
  (define temp-file (make-temporary-file))
  (display-to-file assembly temp-file #:exists 'replace)
  (define result (with-output-to-string
                  (lambda ()
                    (system (string-append "java -jar " (path->string mars) " nc " (path->string temp-file))))))
  (delete-file temp-file)
  (substring result 0 (sub1 (string-length result))))

;; run-x64 : x64String -> String
(define (run-x64 assembly)
  (define temp-file (make-temporary-file "x64-tmp~a.s"))
  (display-to-file assembly temp-file #:exists 'replace)
  (define result (with-output-to-string
                  (lambda ()
                    (system (string-append (path->string build) " " (path->string temp-file) " tmp")))))
  (delete-file temp-file)
  result)

