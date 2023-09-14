#lang racket

(require [for-syntax syntax/parse])

(require smol/lang/semantics)

(provide [except-out [all-from-out smol/lang/semantics]
                     lambda
                     letrec
                     let
                     let*
                     set!
                     #%top
                     #%app])
(provide defvar deffun)
(provide [rename-out (my-lambda lambda)
                     (my-let let)
                     (my-let* let*)
                     (my-letrec letrec)
                     (my-app #%app)
                     (my-set! set!)])

(define dvs (make-parameter (make-hasheq)))

(define (store name v)
  (hash-set! (dvs) name (box v)))

(define (internal-fetch name)
  (hash-ref (dvs) name
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

(define-syntax (my-app stx)
  (syntax-parse stx
    [(_ fun:expr arg:expr ...)
     #'(parameterize ([dvs (hash-copy (dvs))])
         (#%app fun arg ...))]))

(module test racket/base
  (require smol/tests)
  (check-term "smol/dyn-scope" (+ ((lambda (x) x) 2) x) ERROR)
  (check-program "smol/dyn-scope"
    '((deffun (f) x)
      (deffun (g)
        (defvar x 2)
        (f))
      (g))
    '(2)))