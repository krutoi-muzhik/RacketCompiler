#lang racket

(require "utility.rkt")

(provide uniquify)

(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x)
       (Var (dict-ref env x))]
      [(Int n) (Int n)]
      [(Let x e body)
        (define new-x (gensym x))
        (define new-e ((uniquify-exp env) e))
        (define new-env (dict-set env x new-x))
        (Let new-x new-e ((uniquify-exp new-env) body))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))

;; uniquify : Lvar -> Lvar
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp '()) e))]))
