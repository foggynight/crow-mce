#!/usr/bin/env -S csi -q

;; crow.scm - CROW Metacircular Evaluator and REPL in CHICKEN Scheme
;;
;; This will be used to test ideas for CROW, and will be replaced by the
;; interpreter written in C.
;;
;; INTERPRET
;;   ./crow.scm
;;
;; COMPILE
;;   make
;;   ./crow [FILE...]
;;
;; NOTE: Filenames can only be passed as arguments when the program is compiled,
;; otherwise arguments to the CHICKEN interpreter will be included.

(import chicken.format
        chicken.process-context
        simple-exceptions)

;; misc ------------------------------------------------------------------------

(define (null . rest) '())

(define true? (compose not null?))
(define false? null?)
(define (bool->atom bool) (if bool 't '()))

(define (list-cars lst) (map car lst))
(define (list-cadrs lst) (map cadr lst))

(define (zip keys dats)
  (map (lambda (x y) (cons x y)) keys dats))

;; error -----------------------------------------------------------------------

(define (crow-error . args)
  (define fmt (apply string-append (cons "error"
                                         (map (lambda (a) ": ~A") args))))
  (error (apply sprintf (cons fmt args))))

;; environment -----------------------------------------------------------------

(define-constant LIB-PATH "/usr/local/lib/crow/")

(define-record-type env
  (make-env frames) env?
  (frames env-frames)) ; list of frames in the env

(define-record-printer (env e op)
  (let ((frames (env-frames e)))
    (fprintf op "#<env [~A]" (length frames))
    (for-each (lambda (f)
                (display " " op)
                (display f op))
              frames))
  (display ">" op))

(define-record-type frame
  (make-frame name binds) frame?
  (name frame-name)    ; name of the frame
  (binds frame-binds)) ; list of binds in the frame

(define-record-printer (frame f op)
  (fprintf op "#<frame ~A [~A]>"
           (frame-name f)
           (length (frame-binds f))))

;; Fetch the pair whose CAR is SYM from the frames of ENV.
(define (env-fetch sym env)
  (define (&env-fetch frames)
    (if (null? frames)
        (crow-error 'fetch "unbound symbol" sym)
        ((lambda (par)
           (if par par (&env-fetch (cdr frames))))
         (assq sym (frame-binds (car frames))))))
  (&env-fetch (env-frames env)))

;; Get the CDR of the pair whose CAR is SYM from the frames of ENV.
(define (env-lookup sym env)
  (cdr (env-fetch sym env)))

(define (env-bind-pairs keys dats env)
  (make-env (cons (make-frame '? (zip keys dats)) (env-frames env))))

(define (env-bind-formals keys dats env)
  (define (bind k d)
    (cond ((null? k) (if (null? d) '()
                         (crow-error 'env-bind-formals "too many arguments")))
          ((null? d) (if (symbol? k)
                         (cons (cons k '()) '())
                         (crow-error 'env-bind-formals "missing arguments")))
          ((symbol? k) (cons (cons k d) '()))
          (else (cons (cons (car k) (car d))
                      (bind (cdr k) (cdr d))))))
  (let ((frame (make-frame '? (bind keys dats))))
    (make-env (cons frame (env-frames env)))))

;; Insert PAR into the top frame of ENV.
(define (env-insert! par env)
  (define frames (env-frames env))
  (define name (frame-name (car frames)))
  (define binds (frame-binds (car frames)))
  (set-car! frames (make-frame name (cons par binds))))

(define (env-import! name)
  (unless (symbol? name)
    (crow-error 'env-import! "name must be a symbol" name))
  (set! toplevel (make-env (cons (make-frame name '())
                                 (env-frames toplevel))))
  (crow-load (string-append LIB-PATH (symbol->string name) ".crw"))
  (let ((frames (env-frames toplevel)))
    (set! toplevel (make-env (cons (cadr frames)
                                   (cons (car frames)
                                         (cddr frames)))))))

(define (make-toplevel)
  (make-env (list (make-frame 'user '())
                  (make-frame 'core (primitives)))))

;; closure ---------------------------------------------------------------------
;;
;; Closures represent procedures with zero or more free variables bound within
;; an environment, they are represented by lists of the form:
;;
;;   ('closure ((args*) ('body sexp*)) env)
;;
;; Where args, body, and env are from the lambda expression used to create the
;; closure.

;; Create a closure from ARGS and BODY bound in ENV.
(define (closure args body env)
  (list 'closure (list args body) env))

(define (closure? exp)
  (and (pair? exp) (eq? (car exp) 'closure)))

(define closure-args caadr)
(define closure-body cadadr)
(define closure-env caddr)

;; evaluator -------------------------------------------------------------------

;; lst -> (sexp*)
(define (evlist lst env)
  (map (lambda (x) (crow-eval x env)) lst))

;; exp -> sexp
(define (evquasi exp env)
  (define (&quote e) (cons e (evquasi (cdr exp) env)))
  (define (&insert e) (cons (crow-eval e env) (evquasi (cdr exp) env)))
  (define (&splice e) (append (crow-eval e env) (evquasi (cdr exp) env)))
  (cond ((not (pair? exp)) exp)
        ((not (pair? (car exp))) (&quote (car exp)))
        ((eq? (caar exp) 'unquote) (&insert (cadar exp)))
        ((eq? (caar exp) 'unquote-splicing) (&splice (cadar exp)))
        (else (&quote (car exp)))))

;; exp  -> (args sexp*)
;; args -> symbol | (sexp*) | (sexp* . symbol)
(define (evlambda exp env)
  (when (null? exp)
    (crow-error 'evlambda "invalid form" (cons '% exp)))
  (closure (car exp) (cons 'body (cdr exp)) env))

;; clauses -> (clause*)
;; clause  -> (pred sexp*)
(define (evcond clauses env)
  (if (null? clauses) '()
      (let ((c (car clauses)))
        (cond ((not (list? c)) (crow-error 'evcond "invalid clause" c))
              ((eq? (car c) 'else) (crow-eval (cons 'body (cdr c)) env))
              ((false? (crow-eval (car c) env)) (evcond (cdr clauses) env))
              (else (crow-eval (cons 'body (cdr c)) env))))))

;; exp    -> (pred bt bf?)
;; bt, bf -> sexp
(define (evif exp env)
  (unless (< 1 (length exp) 4)
    (crow-error 'evif "invalid form" (cons 'if exp)))
  (let ((bt (cadr exp))
        (bf (if (null? (cddr exp)) '() (caddr exp))))
    (if (true? (crow-eval (car exp) env))
        (crow-eval bt env)
        (if (false? bf) '() (crow-eval bf env)))))

;; exp -> (sexp*)
(define (evand exp last env)
  (if (null? exp) last
      (let ((val (crow-eval (car exp) env)))
        (if (false? val) '()
            (evand (cdr exp) val env)))))

;; exp -> (sexp*)
(define (evor exp last env)
  (if (null? exp) last
      (let ((val (crow-eval (car exp) env)))
        (if (true? val) val
            (evor (cdr exp) val env)))))

;; exp   -> (binds sexp*)
;;        | (symbol binds sexp*)
;; binds -> (bind*)
;; bind  -> (symbol sexp)
(define (evlet exp env)
  (define (validate-binds binds)
    (define (valid? b)
      (and (pair? b) (pair? (cdr b)) (null? (cddr b)) (symbol? (car b))))
    (cond ((null? binds) #t)
          ((not (pair? binds))
           (crow-error 'evlet "invalid binds" binds))
          ((not (valid? (car binds)))
           (crow-error 'evlet "invalid bind" (car binds)))
          (else (validate-binds (cdr binds)))))

  (when (null? exp)
    (crow-error 'evlet "invalid form" (cons 'let exp)))
  (let ((name #f))
    (when (symbol? (car exp))
      (set! name (car exp))
      (set! exp (cdr exp))
      (when (null? exp)
        (crow-error 'evlet "invalid form" (cons 'let exp))))

    (let ((binds (car exp))
          (body (cons 'body (cdr exp))))
      (validate-binds binds)
      (let ((e (env-bind-pairs (list-cars binds)
                               (evlist (list-cadrs binds) env)
                               env)))
        (when name
          (let ((proc (cons 'lambda
                            (cons (list-cars binds)
                                  (cdr exp)))))
            (env-insert! (cons name (crow-eval proc e)) e)))
        (crow-eval body e)))))

;; exp -> (sexp*)
(define (evbody exp env)
  (if (null? exp) '()
      (let ((val (crow-eval (car exp) env)))
        (if (null? (cdr exp)) val
            (evbody (cdr exp) env)))))

;; exp -> (symbol sexp?)
;;      | ((symbol symbol*) sexp*)
;;      | ((symbol symbol* . symbol) sexp*)
(define (evdef exp env)
  (define def
    (cond ((null? exp) (crow-error 'evdef "invalid definition"))
          ((symbol? (car exp)) (cons (car exp) (crow-eval (cadr exp) env)))
          (else (cons (caar exp)
                      (crow-eval (cons 'lambda (cons (cdar exp) (cdr exp)))
                                 env)))))
  (env-insert! def env))

;; exp -> (symbol sexp)
(define (evset! exp env)
  (unless (= (length exp) 2)
    (crow-error 'evset! "invalid form" (cons 'set! exp)))
  (set-cdr! (env-fetch (car exp) env)
            (crow-eval (cadr exp) env)))

(define (evspec exp env)
  (define (eval e) (crow-eval e env))
  (case (car exp)
    ((eval) (eval (eval (cadr exp))))
    ((apply) (crow-apply (eval (cadr exp)) (eval (caddr exp))))
    ((quote) (cadr exp))
    ((quasiquote) (evquasi (cadr exp) env))
    ((% lambda) (evlambda (cdr exp) env))
    ((cond) (evcond (cdr exp) env))
    ((if) (evif (cdr exp) env))
    ((and) (evand (cdr exp) 't env))
    ((or) (evor (cdr exp) '() env))
    ((let) (evlet (cdr exp) env))
    ((body begin) (evbody (cdr exp) env))
    ((def define) (evdef (cdr exp) env) '())
    ((set!) (evset! (cdr exp) env) '())
    (else #f)))

(define (crow-eval exp env)
  (cond ((null? exp) (crow-error 'crow-eval "invalid expression" '()))
        ((symbol? exp) (env-lookup exp env))
        ((or (number? exp) (char? exp) (string? exp)) exp)
        ((list? exp) (let ((val (evspec exp env)))
                       (if val val (crow-apply (crow-eval (car exp) env)
                                               (evlist (cdr exp) env)))))
        (else (crow-error 'crow-eval "invalid expression" exp))))

(define (crow-apply proc args)
  (cond ((primitive? proc) (papply proc args))
        ((closure? proc) (crow-eval (closure-body proc)
                                    (env-bind-formals (closure-args proc)
                                                      args
                                                      (closure-env proc))))
        (else (crow-error 'crow-apply "invalid procedure" proc))))

;; primitive -------------------------------------------------------------------

(define (primitive? proc) (procedure? proc))
(define (papply proc args) (apply proc args))

(include "primitive.scm")

;; repl ------------------------------------------------------------------------

(define (display-banner) (print "CROW MCE (sicp) v0.1.0")
                         (print "(C) 2022 Robert Coffey"))
(define (display-prompt) (display "> "))

(define (crow-repl ip #!optional prompt)
  (when prompt (display-prompt))
  (let ((exp (read ip)))
    (if (eof-object? exp)
        (when prompt (newline) (exit))
        (begin (when (eqv? (peek-char ip) #\newline)
                 (read-char ip)) ; flush newline
               ((lambda (x)
                  (when prompt (write x) (newline)))
                (crow-eval exp toplevel))
               (crow-repl ip prompt)))))

(define (crow-load name)
  (call-with-input-file name crow-repl))

;; main ------------------------------------------------------------------------

(define toplevel (make-toplevel))

(define (main-load #!optional (args (command-line-arguments)))
  (unless (or (null? args) (string=? (car args) "-q"))
    (crow-load (car args)))
  void) ; Must return a procedure to avoid error in WITH-EXN-HANDLER.

(define (main-repl) (crow-repl (current-input-port) #t))

(display-banner)
(letrec ((h-load (lambda (exn)
                   (print (message exn))
                   (exit)))
         (h-repl (lambda (exn)
                   (print (message exn))
                   (with-exn-handler h-repl main-repl))))
  (with-exn-handler h-load main-load)
  (with-exn-handler h-repl main-repl))
