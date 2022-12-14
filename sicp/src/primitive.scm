;; primitive.scm - CROW primitive definitions

(define (primitives) `(
  ;; environment
  (toplevel . ,(lambda () toplevel))

  ;; logic
  (eq? . ,(compose bool->atom eq?))
  (eqv? . ,(compose bool->atom eqv?))
  (equal? . ,(compose bool->atom equal?))
  (not . ,(lambda (x) (if (null? x) 't '())))

  ;; symbol
  (sym? . ,(compose bool->atom symbol?))
  (sym->str . ,symbol->string)

  ;; number
  (num? . ,(compose bool->atom number?))
  (1+ . ,(lambda (n) (+ n 1)))
  (1- . ,(lambda (n) (- n 1)))
  (+ . ,+)
  (- . ,-)
  (* . ,*)
  (/ . ,/)
  (// . ,(lambda args ((compose floor inexact->exact)
                       (apply / args))))
  (= . ,(lambda args (bool->atom (apply = args))))
  (<> . ,(lambda args (bool->atom (not (apply = args)))))
  (> . ,(lambda args (bool->atom (apply > args))))
  (< . ,(lambda args (bool->atom (apply < args))))
  (>= . ,(lambda args (bool->atom (apply >= args))))
  (<= . ,(lambda args (bool->atom (apply <= args))))
  (floor . ,(compose floor inexact->exact))
  (ceil . ,(compose ceiling inexact->exact))
  (trunc . ,(compose truncate inexact->exact))
  (float . ,exact->inexact)

  ;; atom
  (atom? . ,(compose bool->atom atom?))

  ;; cons
  (cons . ,cons)
  (cons? . ,(compose bool->atom pair?))
  (car . ,car)
  (cdr . ,cdr)
  (caar . ,caar)
  (cadr . ,cadr)
  (cdar . ,cdar)
  (cddr . ,cddr)
  (caaar . ,caaar)
  (caadr . ,caadr)
  (cadar . ,cadar)
  (caddr . ,caddr)
  (cdaar . ,cdaar)
  (cdadr . ,cdadr)
  (cddar . ,cddar)
  (cdddr . ,cdddr)
  (caaaar . ,caaaar)
  (caaadr . ,caaadr)
  (caadar . ,caadar)
  (caaddr . ,caaddr)
  (cadaar . ,cadaar)
  (cadadr . ,cadadr)
  (caddar . ,caddar)
  (cadddr . ,cadddr)
  (cdaaar . ,cdaaar)
  (cdaadr . ,cdaadr)
  (cdadar . ,cdadar)
  (cdaddr . ,cdaddr)
  (cddaar . ,cddaar)
  (cddadr . ,cddadr)
  (cdddar . ,cdddar)
  (cddddr . ,cddddr)

  ;; list
  (list . ,list)
  (null? . ,(compose bool->atom null?))
  (list? . ,(compose bool->atom list?))
  (length . ,length)
  (list-ref . ,list-ref)
  (list->vec . ,list->vector)
  (list->str . ,list->string)

  ;; vector
  (vec . ,vector)
  (make-vec . ,(lambda (size . fill)
                 (make-vector size (if (null? fill) '() (car fill)))))
  (vec? . ,(compose bool->atom vector?))
  (vec-len . ,vector-length)
  (vec-ref . ,vector-ref)
  (vec-set! . ,vector-set!)
  (vec->list . ,vector->list)

  ;; character
  (char? . ,(compose bool->atom char?))
  (char= . ,(compose bool->atom char=?))
  (char<> . ,(compose bool->atom not char=?))
  (char< . ,(compose bool->atom char<?))
  (char> . ,(compose bool->atom char>?))
  (char<= . ,(compose bool->atom char<=?))
  (char>= . ,(compose bool->atom char>=?))
  (char-upper . ,char-upcase)
  (char-lower . ,char-downcase)
  (char->int . ,char->integer)
  (int->char . ,integer->char)

  ;; string
  (str . ,string)
  (make-str . ,make-string)
  (str? . ,(compose bool->atom string?))
  (str= . ,(compose bool->atom string=?))
  (str<> . ,(compose bool->atom not string=?))
  (str< . ,(compose bool->atom string<?))
  (str> . ,(compose bool->atom string>?))
  (str<= . ,(compose bool->atom string<=?))
  (str>= . ,(compose bool->atom string>=?))
  (str-ci= . ,(compose bool->atom string-ci=?))
  (str-ci< . ,(compose bool->atom string-ci<?))
  (str-ci> . ,(compose bool->atom string-ci>?))
  (str-ci<= . ,(compose bool->atom string-ci<=?))
  (str-ci>= . ,(compose bool->atom string-ci>=?))
  (str-len . ,string-length)
  (str-ref . ,string-ref)
  (str-set! . ,(compose null string-set!))
  (str-copy . ,string-copy)
  (str-fill! . ,(compose null string-fill!))
  (str-append . ,string-append)
  (substr . ,substring)
  (str->sym . ,string->symbol)
  (str->list . ,string->list)
  (str->num . ,(lambda args
                 ((lambda (x)
                    (if x x (bool->atom x)))
                  (apply string->number args))))

  ;; ports
  (in-port? . ,(compose bool->atom input-port?))
  (out-port? . ,(compose bool->atom output-port?))
  (curr-in-port . ,current-input-port)
  (curr-out-port . ,current-output-port)
  (open-in-file . ,open-input-file)
  (open-out-file . ,open-output-file)
  (close-in-port . ,(compose null close-input-port))
  (close-out-port . ,(compose null close-output-port))

  ;; input
  (eof? . ,(compose bool->atom eof-object?))
  (ready? ,(compose bool->atom char-ready?))
  (read . ,read)
  (read-char . ,read-char)
  (peek-char . ,peek-char)

  ;; output
  (write . ,(compose null write))
  (display . ,(compose null display))
  (print . ,(compose null print))
  (space . ,(lambda () (display #\space)))
  (newline . ,(compose null newline))
  (write-char . ,(compose null write-char))

  ;; misc
  (args . ,command-line-arguments)
  (load . ,(lambda (name) (crow-load name) '()))
  (import . ,(lambda (name) (env-import! name) '()))
  (exit . ,exit)
  (error . ,crow-error)
  (void . ,void)
)) ; primitives
