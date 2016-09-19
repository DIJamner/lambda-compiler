#lang racket

(require racket/system racket/runtime-path)

(provide run-asm)

(define-runtime-path mars "Mars4_5.jar")

(define mars-load-string "MARS 4.5  Copyright 2003-2014 Pete Sanderson and Kenneth Vollmar\n\n")

(define mars-load-string-length (string-length mars-load-string))

;; run-asm : AssemblyString -> String
(define (run-asm assembly)
  (define temp-file (make-temporary-file))
  (display-to-file assembly temp-file #:exists 'replace)
  (define result (with-output-to-string
                  (lambda ()
                    (system (string-append "java -jar " (path->string mars) " " (path->string temp-file))))))
  (delete-file temp-file)
  (if (string=? (substring result 0 mars-load-string-length)
                mars-load-string)
      (substring result mars-load-string-length (sub1 (string-length result)))
      #f))