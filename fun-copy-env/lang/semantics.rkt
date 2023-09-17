#lang racket

#|
In this semantics function remembers everything upon created.
We implement this semantics by saving a timestamp with each variable.
To put it in other way, expressions not denote a history of values.
|#

(require [for-syntax syntax/parse])
(require smol/lang/semantics)

(define current-time 0)

(define (project u)
  (match u
    ['()
     (error "unbound")
    [`((,t ,v) . ,u)
     (if (<= t (current-external-read-time))
         v
         (project u))]]))
(define (inject v)
  `((,current-time ,v)))

(provide [except-out [all-from-out smol/lang/semantics]
                     lambda
                     set!
                     let
                     let*
                     letrec])
(provide [rename-out (my-lambda lambda)
                     (my-set! set!)
                     (my-let let)
                     (my-let* let*)
                     (my-letrec letrec)])
(provide defvar deffun)

(define write-time 0)
(define read-time (make-parameter 0))
(define (lookup tvs expected-t)
  (match tvs
    [emtpy (error "not found")]
    [(cons (cons t v) tvs)
     (if (<= t expected-t)
         v
         (lookup tvs expected-t))]))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(begin
         (define internal-var (list (cons write-time rhs)))
         (define-syntax var
           (make-set!-transformer
              (lambda (stx)
                (syntax-case stx (set!)
                  [var (identifier? (syntax var))
                    (syntax (lookup internal-var (read-time)))]
                  [(set! var e)
                    (syntax
                      (set! internal-var
                        (cons
                          (cons
                            (begin0 write-time (set! write-time (add1 write-time)))
                            e)
                          internal-var)))])))))]))

(define-syntax (deffun stx)
  (syntax-parse stx
    [(_ (fun:id arg:id ...) . body)
     #'(defvar fun
         (my-lambda (arg ...) . body))]))

(define-syntax (my-lambda stx)
  (syntax-parse stx
    [(_ (arg:id ...) body:expr ...+)
     (with-syntax ([(tmp-arg ...)
                    (generate-temporaries #'(arg ...))])
       #'(let ([saved-read-time write-time])
           (lambda (tmp-arg ...)
            (with-parameter ([read-time saved-read-time])
              (defvar arg tmp-arg)
              ...
              body ...))))]))

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

(module test racket/base
  (require smol/tests)
  (check-program "smol/call-by-reference"
    '((defvar x 2)
      (defvar y x)
      (set! y 3)
      x)
    '(3)))