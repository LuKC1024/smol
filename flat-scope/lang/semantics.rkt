#lang racket

#|
This language is like the standard semantics except that
all bindings at things to the one, global environment.
|#

(require [for-syntax syntax/parse])

(require smol/lang/semantics)

(provide [except-out [all-from-out smol/lang/semantics]
                     lambda
                     let
                     let*
                     letrec
                     set!
                     #%top])
(provide defvar deffun)
(provide [rename-out (my-lambda lambda)
                     (my-let let)
                     (my-let* let*)
                     (my-letrec letrec)
                     (my-set! set!)])

(define the-env (make-hasheq))

(define (store name v)
  (hash-set! the-env name (box v)))

(define (internal-fetch name)
  (hash-ref the-env name
            (lambda ()
              (error name "undefined"))))

(define (update name v)
  (define loc (internal-fetch name))
  (set-box! loc v))

(define (fetch name)
  (define loc (internal-fetch name))
  (unbox loc))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(let ([tmp rhs])
         (store 'var tmp))]))

(define-syntax (deffun stx)
  (syntax-parse stx
    [(_ (fname:id arg:id ...) body:expr ...+)
     #'(defvar fname
         (my-lambda (arg ...) body ...))]))

(define-syntax (my-lambda stx)
  (syntax-parse stx
    [(_ (arg:id ...) body:expr ...+)
     (with-syntax ([(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #'(lambda (tmp-arg ...)
           (store 'arg tmp-arg)
           ...
           body ...))]))

(define-syntax-rule (my-app fun arg ...)
  (fun arg ...))
(define-syntax-rule (my-let ([x e] ...) . body)
  (my-app (my-lambda (x ...) . body) e ...))
(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body ...)
     (my-let () body ...)]
    [(my-let* (fst rest ...) body ...)
     (my-let (fst)
       (my-let* (rest ...) body ...))]))
(define-syntax-rule (my-letrec ([x e] ...) . body)
  (my-let ()
    (defvar x e) ... . body))

(define-syntax (my-set! stx)
  (syntax-parse stx
    ([_ var:id val:expr]
     #'(update 'var val))))

(provide (rename-out [handle-id #%top]))

(define-syntax (handle-id stx)
  (syntax-case stx ()
    [(_ . var)
     (with-syntax ([stx stx])
       #'(fetch 'var))]))

(module test racket/base
  (require smol/tests)
  (check-term "smol/flat-scope" (+ ((lambda (x) x) 2) x) 4))