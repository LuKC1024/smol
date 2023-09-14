#lang racket
(provide check-program
         check-term)
(require rackunit)

(define (program->result-string lang program)
  (with-output-to-string
    (thunk
     (define f (make-temporary-file))
     (with-output-to-file f
       (thunk
        (displayln (format "#lang ~a" lang))
        (for ([t program])
          (writeln t)))
       #:exists 'replace)
     (unless (parameterize ([current-error-port (open-output-nowhere)])
               (system* "/Applications/Racket v8.8/bin/racket" f))
       (displayln "ERROR")))))

(define-syntax-rule (check-program lang program results)
  (check-equal? (program->result-string lang program)
                (string-append*
                  (for/list ([r results])
                    (format "~a\n" r)))))

(define-syntax-rule (check-term lang term result)
  (check-program lang '(term) '(result)))