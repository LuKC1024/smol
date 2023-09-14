#lang racket

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
(provide [rename-out (my-λ lambda)
                     (my-let let)
                     (my-let let*)
                     (my-let letrec)
                     (my-set! set!)])

(define current-env (make-parameter (cons (make-hasheq) empty)))

(define (handle-def x v)
  (hash-set! (car (current-env)) x v))

(define (handle-ref x)
  (define (rec env)
    (hash-ref (car env) x
              (lambda ()
                (if (cdr env)
                    (rec (cdr env))
                    (error x "undefined")))))
  (rec (current-env)))

(define (handle-set! x v)
  (define (rec env)
    (cond
      [(hash-has-key? env x)
       (hash-set! env x v)]
      [(cdr env)
       (rec (cdr env))]
      [else
       (error x "undefined")]))
  (rec (current-env)))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(let ([tmp rhs])
         (handle-def 'var tmp))]))

(define-syntax (deffun stx)
  (syntax-parse stx
    [(_ (fname:id arg:id ...) body:expr ...+)
     #'(defvar fname
         (my-λ (arg ...) body ...))]))

(define-syntax (my-λ stx)
  (syntax-parse stx
    [(_ (arg:id ...) body:expr ...+)
     (with-syntax ([(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #'(let ([my-env (current-env)])
           (lambda (tmp-arg ...)
            (parameterize ([current-env my-env])
              (handle-def 'arg tmp-arg)
              ...
              body ...))))]))

(define-syntax (my-let stx)
  (syntax-parse stx
    ([_ ([var:id val:expr] ...) body:expr ...+]
     #'(my-app (my-λ (var ...) body ...) val ...))))

(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body ...)
     (my-let () body ...)]
    [(my-let* (fst rest ...) body ...)
     (my-let (fst)
       (my-let* (rest ...) body ...))]))

(define-syntax (my-set! stx)
  (syntax-parse stx
    ([_ var:id val:expr]
     #'(handle-set! 'var val))))

(provide (rename-out [handle-id #%top]))

(define-syntax (handle-id stx)
  (syntax-case stx ()
    [(_ . var)
     #'(handle-ref 'var)]))

(module test racket/base
  (require smol/tests)
  (check-program "smol/allow-redefine"
    '((defvar x 2)
      x
      (defvar x 3)
      x)
    '(2 3)))