#lang racket

(require [for-syntax syntax/parse])

(require smol/lang/semantics)

(provide [except-out [all-from-out smol/lang/semantics]
                     lambda
                     let
                     let*
                     letrec
                     set!
                     #%app])
(provide defvar deffun)
(provide [rename-out (my-lambda lambda)
                     (my-let let)
                     (my-let* let*)
                     (my-letrec letrec)
                     (my-set! set!)
                     (my-app #%app)])

(define current-env (make-parameter (cons (make-hasheq) empty)))
(define-syntax-rule (my-app fun arg ...)
  (let ([f (as-val fun)])
    (if (ud-proc? f)
        (as-val (f (as-box arg) ...))
        (f (as-val arg) ...))))

(define (as-val v)
  (if (box? v) (unbox v) v))

(define (ensure-box x)
  (if (box? x)
      (box (unbox x))
      (box x)))

(define-syntax (as-box stx)
  (syntax-parse stx
    [(_ x:id)
     #'x]
    [(_ e)
     #'(ensure-box e)]))

(define-syntax (my-set! stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(set-box! var (as-val rhs))]))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(define var (as-box rhs))]))

(define-syntax (deffun stx)
  (syntax-parse stx
    [(_ (fun:id arg:id ...) . body)
     #'(defvar fun
         (my-lambda (arg ...) . body))]))

(define-syntax (my-lambda stx)
(syntax-parse stx
    [(_ (arg:id ...) . body)
     #'(ud-proc
         (lambda (arg ...)
           (as-val (let () . body))))]))

(struct ud-proc (base)
    #:property prop:procedure
               (struct-field-index base))

(define-syntax (my-let stx)
  (syntax-parse stx
    ([_ ([var:id val:expr] ...) body:expr ...+]
     #'(my-app (my-lambda (var ...) body ...) val ...))))
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

(current-print (let ([p (current-print)])
                 (lambda (v) (p (as-val v)))))

(module test racket/base
  (require smol/tests)
  (check-program "smol/call-by-reference"
    '((defvar x 2)
      (defvar y x)
      (set! y 3)
      x)
    '(3)))