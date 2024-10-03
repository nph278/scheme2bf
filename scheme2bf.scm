(use-modules (srfi srfi-1)
             (srfi srfi-26))

;; l0: SIMPLE BYTE TAPE
;;
;; move n       -- Move right n cells. If negative, move left.
;; add n/char   -- Add n to current cell, or char's code.
;; reset        -- Reset current cell to 0. More performant on lower (<128) cells.
;; reset-up     -- Reset current cell to 0. More performant on higher (>127) cells.
;; loop . exprs -- Loop expressions while current cell is not 0.

(define (l0->bf expr)
  (cond ((not (list? expr)) (error "l0 ERROR: non-list instruction"))
        ((nil? expr) (error "l0 ERROR: nil instruction"))
        (else (case (first expr)
                ((add) (cond ((nil? (drop expr 1)) (error "l0 ERROR: empty add instruction"))
                             ((not (nil? (drop expr 2))) (error "l0 ERROR: too many arguments in add instruction"))
                             ((number? (second expr)) (bf-number (second expr)))
                             ((character? (second expr)) (bf-number (char->integer (second expr))))
                             (else (error "l0 ERROR: invalid add instruction argument type"))))
                ((move) (cond ((nil? (drop expr 1)) (error "l0 ERROR: empty move instruction"))
                              ((not (nil? (drop expr 2))) (error "l0 ERROR: too many arguments in move instruction"))
                              ((number? (second expr)) (bf-move (second expr)))
                              (else (error "l0 ERROR: invalid move instruction argument type"))))
                ((reset) (cond ((not (nil? (drop expr 1))) (error "l0 ERROR: too many arguments in reset instruction"))
                               (else "[-]")))
                ((reset-up) (cond ((not (nil? (drop expr 1))) (error "l0 ERROR: too many arguments in reset-up instruction"))
                                  (else "[+]")))
                ((loop) (string-append "[" (l0s->bf (drop 1 expr)) "]"))
                (else (error "l0 ERROR: unknown instruction" (first expr)))))))

(define (l0s->bf exprs)
  (apply string-append (map l0->bf exprs)))


(define (bf-number n)
  (let ((n (modulo n 256)))
    (if (< n 128)
        (make-string n #\+)
        (make-string (- 256 n) #\-))))

(define (bf-move n)
  (if (> n 0)
      (make-string n #\>)
      (make-string (- n) #\<)))


(define (compile l0)
  (let* ((bf (l0s->bf l0)))
    bf))

(define example '((add 254) (move 10) (move -10) (reset) (reset-up)))
(display (compile example))
(newline)
