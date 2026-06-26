#lang racket

(require racket/fixnum
				 "utility.rkt"
				 "interp-Lint.rkt")

(provide pe-Lint)

; partial evaluate unary minus
(define (pe-neg r)
	(match r
		[(Int n) (Int (fx- 0 n))]
		[else (Prim '- (list r))]))

; partial evaluate addition
(define (pe-add r1 r2)
	(match* (r1 r2)
		[((Int n1) (Int n2)) (Int (fx+ n1 n2))]
		[(_ _) (Prim '+ (list r1 r2))]))

; partial evaluate substraction
(define (pe-sub r1 r2)
	(match* (r1 r2)
		[((Int n1) (Int n2)) (Int (fx- n1 n2))]
		[(_ _) (Prim '- (list r1 r2))]))

; partial evaluate Lint expression
(define (pe-exp e)
	(match e
		[(Int n) (Int n)]
		[(Prim 'read '()) (Prim 'read '())]
		[(Prim '- (list e1)) (pe-neg (pe-exp e1))]
		[(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]
		[(Prim '- (list e1 e2)) (pe-sub (pe-exp e1) (pe-exp e2))]))

; partial evaluate Lint program
(define (pe-Lint p)
	(match p
		[(Program '() e) (Program '() (pe-exp e))]))

; ---------------------------------------------------------------------------
; Run with:  raco test part-eval.rkt
(module+ test
  (require rackunit
  				 rackunit/text-ui)

	; The main invariant of the partial evaluator:
	; for any program p, interpreting the original program
	; must agree with interpreting the partially evaluated one.
	(define (semantic-test-pe p)
    (equal? (interp-Lint p)
            (interp-Lint (pe-Lint p))))
	 
	(define pe-Lint-tests
	  (test-suite
	   "pe-Lint: semantics preservation"
	 
	   (test-case "nested unary minus and addition"
	     (semantic-test-pe
	       (parse-program `(program () (+ 10 (- (+ 5 3)))))))
	 
	   (test-case "chain of additions"
	     (semantic-test-pe
	       (parse-program `(program () (+ 1 (+ 3 1))))))
	 
	   (test-case "minus of a sum with negation"
	     (semantic-test-pe
	       (parse-program `(program () (- (+ 3 (- 5)))))))))

	(run-tests pe-Lint-tests))
