#lang racket

(require racket/fixnum)
(require "utility.rkt")

(provide Lint%)

(define Lint%
	(class object%
		(super-new)

		(define/public ((interp-exp env) e)
			(match e
				[(Int n) n]
				[(Prim 'read '())
					(define r (read))
					(cond
						[(fixnum? r) r]
						[else (error 'interp-exp "expected an integer" r)])]
				[(Prim '- (list e)) (fx- 0 ((interp-exp env) e))]
				[(Prim '+ (list e1 e2))
					(fx+ ((interp-exp env) e1) ((interp-exp env) e2))]
				[(Prim '- (list e1 e2))
					(fx- ((interp-exp env) e1) ((interp-exp env) e2))]))

		(define/public (interp-program p)
			(match p
				[(Program '() e) ((interp-exp '()) e)]))
	))


(define (interp-Lint p)
	(send (new Lint%) interp-program p))

; ---------------------------------------------------------------------------
; Run with:  raco test Lint.rkt
(module+ test
	(require rackunit rackunit/text-ui)

	; sample AST nodes
	(define eight (Int 8))
	(define neg-eight (Prim '- (list eight)))
 
	(define interp-tests
	  (test-suite
	   "interp-Lint-class: interp-program"
	   (test-case "single integer"
	     (check-equal? (interp-Lint (Program '() (Int 8))) 8))
	   (test-case "unary negation"
	     (check-equal? (interp-Lint (Program '() neg-eight)) -8))
	   (test-case "addition"
	     (check-equal? (interp-Lint (Program '() (Prim '+ (list (Int 8) (Int 10))))) 18))
	   (test-case "binary subtraction"
	     (check-equal? (interp-Lint (Program '() (Prim '- (list (Int 10) (Int 3))))) 7))
	   (test-case "nested expression"
	     (check-equal?
	      (interp-Lint (Program '() (Prim '+ (list (Int 10)
	                                               (Prim '- (list (Prim '+ (list (Int 5) (Int 3)))))))))
	      2))))

	(run-tests interp-tests))
