#lang racket

(require [for-syntax syntax/parse])
(require [for-syntax racket/match])

(require smol/lang/semantics)

(provide [except-out [all-from-out smol/lang/semantics]
                     lambda
                     letrec
                     let
                     let*
                     #%module-begin])
(provide [rename-out (my-lambda lambda)
                     (my-let let)
                     (my-let* let*)
                     (my-letrec letrec)
                     (my-module-begin #%module-begin)])

(define-syntax (my-module-begin stx)
  (syntax-parse stx
    [(_ term ...)
     #`(#%module-begin #,@(block->let* #t (list) (syntax->list #'(term ...))))]))

(define-syntax (my-lambda stx)
  (syntax-parse stx
    [(_ (arg:id ...) body:expr ...+)
     #`(lambda (arg ...) #,@(block->let* #f (syntax->list #'(arg ...)) (syntax->list #'(body ...))))]))

(begin-for-syntax
  (define (block->let* top-level? ts)
    (match ts
      [`() `()]
      [`(,t . ,ts)
       (syntax-parse t
       	#:datum-literals (defvar deffun)
        [(defvar y e)
         ]
        [(deffun (y arg ...) . body)
         ]
        [any
         #`(begin
             (pretty-print any)
             #,(block->let* top-level? ts))])])))

(define-syntax (my-let stx)
  (syntax-parse stx
    ([_ ([var:id val:expr] ...) body:expr ...+]
     #'((my-lambda (var ...) body ...) val ...))))
(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body ...)
     (my-let () body ...)]
    [(my-let* (fst rest ...) body ...)
     (my-let (fst)
       (my-let* (rest ...) body ...))]))
(define-syntax my-letrec
  (syntax-rules ()
    [(my-letrec ([x e] ...) . body)
     (my-let ()
       (defvar x e) ... . body)]))

(module test racket/base
  (require smol/tests)
  (check-program "smol/allow-redefine"
    '((defvar x 2)
      x
      (defvar x 3)
      x)
    '(2 3)))