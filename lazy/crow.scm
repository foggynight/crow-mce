;; crow.scm - CROW MCE (lazy)
;; Copyright (C) 2022 Robert Coffey
;; Released under the MIT license.

(import (chicken format)
        simple-exceptions)

;; env -------------------------------------------------------------------------

(define (env-user) '())
(define (env-core) '((x . 1) (y . 2) (z . 3)))
(define (env-toplevel) (list (env-user) (env-core)))

(define (env-lookup env sym)
  (when (null? env)
    (error 'env-lookup "unbound symbol" sym))
  (define frame (car env))
  (define pair (assoc sym frame))
  (if pair
      (cdr pair)
      (env-lookup (cdr env) sym)))

(define (env-insert! env sym val)
  (set-car! env (cons (cons sym val) (car env))))

;; eval ------------------------------------------------------------------------

(define (eval-special-define! name body env)
  (env-insert! env name (crow-eval! (car body) env)))

(define (eval-special! e env)
  (define name (car e))
  (define spec #f)
  (define val (void))
  (when (symbol? name)
    (set! spec #t)
    (let ((body (cdr e)))
      (case name
        ((quote) (set! val (car body)))
        ((def!) (eval-special-define! (car body) (cdr body) env)))))
  (values spec val))

(define (crow-eval! e env)
  (cond ((null? e) (error 'crow-eval "invalid expression" e))  ; null
        ((or (number? e) (char? e) (string? e) (vector? e)) e) ; literal
        ((symbol? e) (env-lookup env e))                       ; symbol
        ((list? e)                                             ; list
         (let-values (((spec val) (eval-special! e env)))
           (if spec val)))
        ((pair? e) e)                                          ; cons
        (else (error 'crow-eval "unknown expression type"))))

;; main ------------------------------------------------------------------------

(define toplevel (env-toplevel))

(define (main #!optional (eval-count 0))
  (printf "#;~A> " eval-count)
  (define sexp (read))
  (define res (crow-eval! sexp toplevel))
  (unless (eq? res (void)) (write res) (newline))
  (main (+ eval-count 1)))

(print "CROW MCE (lazy) v0.0.0")
(print "(C) Robert Coffey 2022")

;;(letrec ((handler (lambda (exn)
;;                    (print (message exn))
;;                    (with-exn-handler handler main))))
;;  (with-exn-handler handler main))

(main)
