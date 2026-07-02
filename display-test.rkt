#lang racket

(require 	"utility.rkt"
					"rco.rkt"
					"uniquify.rkt")

(define (print-program label p)
  (printf "~a\n" label)
  (printf "~a\n" (make-string (string-length label) #\-))
  (parameterize ([AST-output-syntax 'concrete-syntax])
    (displayln p))
  (newline))

(define (run-file filename)
  (unless (file-exists? filename)
    (error 'display-tests "No file: ~a" filename))
  (define prog (read-program filename))

  (print-program "Src:" prog)

  (define uniq (uniquify prog))
  (print-program "Post uniquify" uniq)

  (define rco (remove-complex-operands uniq))
  (print-program "Post remove-complex-operands" rco))

(define (main)
  (display "Type filename: ")
  (flush-output)
  (define input (read-line))
  (cond
    [(eof-object? input)
     (displayln "No input")]
    [else
     (run-file input)]))

(main)
