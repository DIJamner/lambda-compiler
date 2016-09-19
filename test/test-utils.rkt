#lang racket

(require racket/system racket/runtime-path)

(provide run-asm)

(define-runtime-path mars "Mars4_5.jar")

;; run-asm : AssemblyString -> String
(define (run-asm assembly)
  (define temp-file (make-temporary-file))
  (display-to-file assembly temp-file #:exists 'replace)
  (define result (with-output-to-string
                  (lambda ()
                    (system (string-append "java -jar " (path->string mars) " nc " (path->string temp-file))))))
  (delete-file temp-file)
  (substring result 0 (sub1 (string-length result))))