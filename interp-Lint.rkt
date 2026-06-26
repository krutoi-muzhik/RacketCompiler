#lang racket

(require racket/fixnum)
(require "utility.rkt")

(provide leaf is-Lint interp-Lint)

; returns #t if node is leaf in AST
(define (leaf node)
	(match node
		[(Int n) #t]
		[(Prim 'read '()) #t]
		[(Prim '- (list e1)) #f]
		[(Prim '+ (list e1 e2)) #f]
		[(Prim '- (list e1 e2)) #f]))

; return #t if ast is Lint expression
(define (is-exp ast)
	(match ast
		[(Int n) #t]
		[(Prim 'read '()) #t]
		[(Prim '- (list e)) (is-exp e)]
		[(Prim '+ (list e1 e2)) (and (is-exp e1) (is-exp e2))]
		[(Prim '- (list e1 e2)) (and (is-exp e1) (is-exp e2))]
		[else #f]))

; returns #t if ast is Lint program
(define (is-Lint ast)
	(match ast
		[(Program '() e) (is-exp e)]
		[else #f]))

; interpret expression
(define (interp-exp e)
	(match e
		[(Int n) n]
		[(Prim 'read '())
			(define r (read))
			(cond 
				[(fixnum? r) r]
				[else (error 'interp-exp "read expected an integer" r)]	
			)]
		[(Prim '- (list e))
			(define v (interp-exp e))
			(fx- 0 v)]
		[(Prim '+ (list e1 e2))
			(define v1 (interp-exp e1))
			(define v2 (interp-exp e2))
			(fx+ v1 v2)]
		[(Prim '+ (list e1 e2))
			(define v1 (interp-exp e1))
			(define v2 (interp-exp e2))
			(fx+ v1 v2)]))

; interpret program
(define (interp-Lint p)
	(match p
		[(Program '() e) (interp-exp e)]))

; ---------------------------------------------------------------------------
; Run with:  raco test interp-Lint.rkt
(module+ test
	(require rackunit 
					 rackunit/text-ui)
 
	; sample AST nodes
	(define eight (Int 8))
	(define neg-eight (Prim '- (list eight)))
	(define rd (Prim 'read '()))
	(define ast1_1 (Prim '+ (list rd neg-eight)))
 
	(define leaf-tests
	  (test-suite
	   "leaf"
	   (test-case "read is a leaf"
	     (check-true (leaf rd)))
	   (test-case "Int is a leaf"
	     (check-true (leaf eight)))
	   (test-case "unary minus is not a leaf"
	     (check-false (leaf neg-eight)))
	   (test-case "binary plus is not a leaf"
	     (check-false (leaf (Prim '+ (list (Int 1) (Int 2))))))))
 
	(define is-exp-tests
	  (test-suite
	   "is-exp"
	   (test-case "lone read is a valid expression"
	     (check-true (is-exp rd)))
	   (test-case "nested arithmetic is a valid expression"
	     (check-true (is-exp ast1_1)))
	   (test-case "non-Lint operator is rejected"
	     (check-false (is-exp (Prim '* (list (Int 2) (Int 3))))))))
 
	(define is-Lint-tests
	  (test-suite
	   "is-Lint"
	   (test-case "well-formed program"
	     (check-true (is-Lint (Program '() ast1_1))))
	   (test-case "program with binary minus"
	     (check-true
	      (is-Lint (Program '() (Prim '- (list (Prim 'read '())
	                                            (Prim '+ (list (Int 8) (Int 10)))))))))
	   (test-case "non-program is rejected"
	     (check-false (is-Lint (Int 8))))))
 
	; interpreter tests use read-free programs so raco test does not block on stdin
	(define interp-tests
	  (test-suite
	   "interp-Lint"
	   (test-case "single integer"
	     (check-equal? (interp-Lint (Program '() (Int 8))) 8))
	   (test-case "unary negation"
	     (check-equal? (interp-Lint (Program '() neg-eight)) -8))
	   (test-case "addition"
	     (check-equal? (interp-Lint (Program '() (Prim '+ (list (Int 8) (Int 10))))) 18))
	   (test-case "nested expression"
	     (check-equal? (interp-Lint (Program '() (Prim '+ (list (Int 10)
	                                                            (Prim '- (list (Prim '+ (list (Int 5) (Int 3)))))))))
	                   2))))
 
	(run-tests leaf-tests)
	(run-tests is-exp-tests)
	(run-tests is-Lint-tests)
	(run-tests interp-tests))
