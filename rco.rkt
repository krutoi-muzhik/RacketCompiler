#lang racket

(require "utility.rkt")

(provide remove-complex-operands)

(define (rco-atom e)
  (match e
    [(Int n) (values (Int n) (list))]
    [(Var x) (values (Var x) (list))]
    [(Let x exp body)
      (define tmp-name (gensym 'tmp))
      (values (Var tmp-name) (list (cons tmp-name (rco-exp e))))]
    [(Prim op es)
      (define tmp-name (gensym 'tmp))
      (values (Var tmp-name) (list (cons tmp-name (rco-exp e))))]))

(define (wrap bindings body)
  (foldl (lambda (b acc) (Let (car b) (cdr b) acc)) body bindings))

(define (rco-exp e)
  (match e
    [(Int n) (Int n)]
    [(Var x) (Var x)]
    [(Let x exp body)
      (Let x (rco-exp exp) (rco-exp body))]
    [(Prim op es)
      (define-values (atoms bind-list)
        (for/lists (atoms binds)
                   ([arg (in-list es)])
          (rco-atom arg)))
      (wrap (append* bind-list) (Prim op atoms))]))

;; remove-complex-opera* : Lvar -> Lvar^mon
(define (remove-complex-operands p)
  (match p
    [(Program info e) (Program info (rco-exp e))]))
