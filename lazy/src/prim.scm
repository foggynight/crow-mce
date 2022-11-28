(define (primitives) `(
  ;; symbol
  (symbol? . ,symbol?)
  (symbol->string . ,symbol->string)

  ;; number
  (number? . ,number?)
  (+ . ,+)
  (- . ,-)
  (* . ,*)
  (/ . ,/)
  (= . ,=)
  (<> . ,(compose not =))
  (< . ,<)
  (> . ,>)
  (<= . ,<=)
  (>= . ,>=)
  (floor . ,floor)
  (ceil . ,ceiling)
  (trunc . ,truncate)
  (round . ,round)
  (float . ,exact->inexact)
  (exact . ,inexact->exact)

  ;; cons
  (cons . ,cons)
  (cons? . ,pair?)
  (car . ,car)
  (cdr . ,cdr)

  ;; list
  (list . ,list)
  (null? . ,null?)
  (list? . ,list?)
  (list-length . ,length)
  (list-ref . ,list-ref)

  ;; vector

  ;; char

  ;; string

  ;; IO
  (write . ,write)
  (display . ,display)
  (print . ,print)
  (newline . ,newline)
  ))
