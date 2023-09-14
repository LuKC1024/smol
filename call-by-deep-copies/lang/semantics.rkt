#lang racket

(require [for-syntax syntax/parse])

(require smol/lang/semantics)

(provide [except-out [all-from-out smol/lang/semantics]
                     let
                     let*
                     letrec
                     set!])
(provide defvar deffun)
(provide [rename-out (my-let let)
                     (my-let* let*)
                     (my-letrec letrec)
                     (my-set! set!)])

(define-syntax-rule (my-app fun arg ...)
  (#%app (deep-copy fun) (deep-copy arg) ...))

(define-syntax (my-set! stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(set! var (deep-copy rhs))]))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(define var (deep-copy rhs))]))

(define-syntax (deffun stx)
  (syntax-parse stx
    [(_ (fun:id arg:id ...) . body)
     #'(defvar fun
         (lambda (arg ...) . body))]))

(define-syntax (my-let stx)
  (syntax-parse stx
    ([_ ([var:id val:expr] ...) body:expr ...+]
     #'(my-app (lambda (var ...) body ...) val ...))))
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

(define (deep-copy v)
  (cond
    [(vector? v)
     (vector-map deep-copy v)]
    [else v]))

(module test racket/base
  (require smol/tests)
  (check-program "smol/call-by-deep-copies"
    '((defvar x (vec 1 2 3))
      (defvar y x)
      (vec-set! y 0 42)
      (vec-ref x 0))
    '(1)))