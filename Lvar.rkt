#lang racket

(require "utility.rkt"
				 "Lint.rkt")

(provide Lvar%)

(define Lvar%
	(class Lint%
		(super-new)

		(define/override ((interp-exp env) e)
			(match e
				[(Var x) (dict-ref env x)]
				[(Let x e body)
					(define new-env (dict-set env x ((interp-exp env) e)))
					((interp-exp new-env) body)]
				[else ((super interp-exp env) e)]))
	))

(define (interp-Lvar p)
	(send (new Lvar%) interp-program p))

; ---------------------------------------------------------------------------
; Run with:  raco test Lvar.rkt
(module+ test
	(require rackunit rackunit/text-ui)

	; sample AST nodes
	(define eight (Int 8))
	(define neg-eight (Prim '- (list eight)))

	(define interp-tests
	  (test-suite
	   "interp-Lvar-class: interp-program"
	   (test-case "single integer"
	     (check-equal? (interp-Lvar (Program '() (Int 8))) 8))))


	(run-tests interp-tests))

