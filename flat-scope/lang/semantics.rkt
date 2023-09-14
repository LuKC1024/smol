#lang racket

#|
This language is like the standard semantics except that
all bindings at things to the one, global environment.
|#

(require [for-syntax syntax/parse])

(require smol/lang/semantics)

(provide [except-out [all-from-out smol/lang/semantics]
                     lambda
                     letrec
                     let
                     let*
                     set!
                     #%top])
(provide defvar deffun)
(provide [rename-out (dyn-lambda lambda)
                     (dyn-let let)
                     (dyn-let let*)
                     (dyn-let letrec)
                     (dyn-set! set!)])

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
         (dyn-lambda (arg ...) body ...))]))

(define-syntax (dyn-lambda stx)
  (syntax-parse stx
    [(_ (arg:id ...) body:expr ...+)
     (with-syntax ([(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #'(lambda (tmp-arg ...)
           (store 'arg tmp-arg)
           ...
           body ...))]))

(define-syntax (dyn-let stx)
  (syntax-parse stx
    ([_ ([var:id val:expr] ...) body:expr ...+]
     #'(dyn-app (dyn-lambda (var ...) body ...) val ...))))

(define-syntax dyn-let*
  (syntax-rules ()
    [(dyn-let* () body ...)
     (dyn-let () body ...)]
    [(dyn-let* (fst rest ...) body ...)
     (dyn-let (fst)
       (dyn-let* (rest ...) body ...))]))

(define-syntax (dyn-set! stx)
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