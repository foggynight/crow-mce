;; crow.scm - CROW MCE (lazy)
;; Copyright (C) 2022 Robert Coffey
;; Released under the MIT license.

(import (chicken format)
        (chicken process-context)
        simple-exceptions)

;; proc ------------------------------------------------------------------------

(define-record-type proc
  (make-proc args body env)
  proc?
  (args proc-args proc-args-set!)
  (body proc-body proc-body-set!)
  (env proc-env proc-env-set!))

;; Determine if `proc` is a CHICKEN Scheme procedure.
(define (primitive? proc) (procedure? proc))

(include "prim.scm") ; Define primitive procedures.

;; env -------------------------------------------------------------------------

(define (env-user) '())
(define (env-core) (primitives))
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

(define (env-bind-formals env args vals)
  (define (bind-formals args vals)
    (cond ((null? args)
           (if (null? vals)
               '()
               (error 'env-bind-formals "too many arguments")))
          ((null? vals) (error 'env-bind-formals "missing arguments"))
          (else (cons (cons (car args) (car vals))
                      (bind-formals (cdr args) (cdr vals))))))
  (cons (bind-formals args vals) env))

;; eval ------------------------------------------------------------------------

(define (eval-list! lst env)
  (map (lambda (e) (crow-eval! e env)) lst))

(define (eval-special-body! body env)
  (if (null? body)
      (void)
      (let loop! ((b body))
        (let ((val (crow-eval! (car b) env)))
          (if (null? (cdr b))
              val
              (loop! (cdr b)))))))

(define (eval-special-cond! clauses env)
  (if (null? clauses)
      (void)
      (let ((pred (caar clauses))
            (body (cdar clauses)))
        (cond ((eq? 'else pred) (crow-eval! (cons 'body body) env))
              ((crow-eval! pred env) (crow-eval! (cons 'body body) env))
              (else (eval-special-cond! (cdr clauses) env))))))

(define (eval-special-def! name body env)
  (env-insert! env name (crow-eval! (car body) env)))

(define (eval-special-lambda args body env)
  (make-proc args (cons 'body body) env))

(define (eval-special! e env)
  (define name (car e))
  (define spec #t)    ; Is `e` a special form?
  (define val (void)) ; If `spec`: `val` is result of form, else: void.
  (if (not (symbol? name))
      (set! spec #f)
      (let ((body (cdr e)))
        (case name
          ((body) (set! val (eval-special-body! body env))) ; TODO: Add a body! version?
          ((cond) (set! val (eval-special-cond! body env)))
          ((def!) (eval-special-def! (car body) (cdr body) env))
          ((lambda) (set! val (eval-special-lambda (car body) (cdr body) env)))
          ((quote) (set! val (car body)))
          (else (set! spec #f)))))
  (values spec val))

(define (crow-eval! e env)
  (cond ((null? e) (error 'crow-eval! "invalid expression" e))
        ((or (boolean? e) (number? e) (char? e) (string? e) (vector? e)) e)
        ((symbol? e) (env-lookup env e))
        ((list? e)
         (let-values (((spec val) (eval-special! e env)))
           (if spec val (crow-apply! (crow-eval! (car e) env)
                                     (eval-list! (cdr e) env)))))
        ((pair? e) e)
        (else (error 'crow-eval! "unknown expression type"))))

(define (crow-apply! proc args)
  (cond ((primitive? proc) (apply proc args))
        ((proc? proc) (crow-eval! (proc-body proc)
                                  (env-bind-formals (proc-env proc)
                                                    (proc-args proc)
                                                    args)))
        (else)))

;; main ------------------------------------------------------------------------

(define toplevel (env-toplevel))

(define should-print #t)

(define (main #!optional (eval-count 0))
  (when should-print
    (printf "#;~A> " eval-count))
  (define sexp (read))
  (unless (eof-object? sexp)
    (let ((res (crow-eval! sexp toplevel)))
      (unless (eq? res (void)) (write res) (newline)))
    (main (+ eval-count 1))))

;;(letrec ((handler (lambda (exn)
;;                    (print (message exn))
;;                    (with-exn-handler handler main))))
;;  (with-exn-handler handler main))

(let* ((args (command-line-arguments))
       (argc (length args)))
  (if (> argc 0)
      (begin (set! should-print #f)
             (with-input-from-file (car args) main))
      (begin (print "CROW MCE (lazy) v0.0.0")
             (print "(C) Robert Coffey 2022")
             (main))))
