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

;; closure ---------------------------------------------------------------------

(define-record-type closure
  (make-closure args body env)
  closure?
  (args closure-args closure-args-set!)
  (body closure-body closure-body-set!)
  (env closure-env closure-env-set!))

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

(define (eval-special-def! name body env)
  (env-insert! env name (crow-eval! (car body) env)))

(define (eval-special-lambda args body env)
  (make-closure args (cons 'body body) env))

(define (eval-special! e env)
  (define name (car e))
  (define spec #t)    ; Is E a special form?
  (define val (void)) ; If SPEC: VAL is result of form, else: void.
  (if (not (symbol? name))
      (set! spec #f)
      (let ((body (cdr e)))
        (case name
          ((body) (set! val (eval-special-body! body env)))
          ((def!) (eval-special-def! (car body) (cdr body) env))
          ((lambda) (set! val (eval-special-lambda (car body) (cdr body) env)))
          ((quote) (set! val (car body)))
          (else (set! spec #f)))))
  (values spec val))

(define (crow-eval! e env)
  (cond ((null? e) (error 'crow-eval "invalid expression" e))  ; null
        ((or (number? e) (char? e) (string? e) (vector? e)) e) ; literal
        ((symbol? e) (env-lookup env e))                       ; symbol
        ((list? e)                                             ; list
         (let-values (((spec val) (eval-special! e env)))
           (if spec val (crow-apply! (crow-eval! (car e) env)
                                     (eval-list! (cdr e) env)))))
        ((pair? e) e)                                          ; cons
        (else (error 'crow-eval "unknown expression type"))))

(define (crow-apply! proc args)
  (cond ;((primitive? proc))
        ((closure? proc) (crow-eval! (closure-body proc)
                                     (env-bind-formals (closure-env proc)
                                                       (closure-args proc)
                                                       args)))
        (else)))

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
