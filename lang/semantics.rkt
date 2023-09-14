#lang racket

(require [for-syntax syntax/parse])

(provide
  set!
  let
  let*
  letrec
  lambda
  if
  cond
  begin
  error
  + - * /
  < <= > >=
  #%module-begin
  #%top-interaction
  #%datum
  #%top
  #%app
  (all-defined-out))

(define ++ string-append)

; equality

(define = eq?)
(define equal? eq?)

; vector operators

(define (vec . args)
  (apply vector args))
(define mvec vec)
(define ivec vec)
(define vec-set! vector-set!)
(define vec-ref vector-ref)
(define vec-len vector-length)

; pair operators

(define (pair a b)
  (vec a b))
(define mpair pair)
(define ipair pair)
(define (left p)
  (unless (= (vec-len p) 2)
    (error "left: argument must be a pair: ~a" p))
  (vec-ref p 0))
(define (right p)
  (unless (= (vec-len p) 2)
    (error "right: argument must be a pair: ~a" p))
  (vec-ref p 1))
(define (set-left! p v)
  (unless (= (vec-len p) 2)
    (error "set-left!: argument must be a pair: ~a" p))
  (vec-set! p 0 v))
(define (set-right! p v)
  (unless (= (vec-len p) 2)
    (error "set-right!: argument must be a pair: ~a" p))
  (vec-set! p 1 v))
(define (pair? v) (and (vector? v) (= (vec-len v) 2)))

(define-syntax (defvar stx)
  (syntax-parse stx
    [(_ var:id rhs:expr)
     #'(define var rhs)]))

(define-syntax (deffun stx)
  (syntax-parse stx
    [(_ (fname:id arg:id ...) . body)
     #'(define (fname arg ...) . body)]))
