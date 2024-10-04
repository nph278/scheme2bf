(use-modules (srfi srfi-1))

;; L0: SIMPLE BYTE TAPE
;;
;; move n        -- Move right n bytes. If negative, move left.
;; add n         -- Add n to current byte.
;; reset         -- Reset current byte to 0. More performant on lower (<128) bytes.
;; input         -- Input to current byte.
;; output        -- Output current byte.
;; while . exprs -- Loop expressions while current byte is not 0.

(define (make-l0)
  (define (bf-number n)
    (let ((n (modulo n 256)))
      (if (< n 128)
          (make-string n #\+)
          (make-string (- 256 n) #\-))))

  (define (bf-move n)
    (if (> n 0)
        (make-string n #\>)
        (make-string (- n) #\<)))

  (define (compile-one expr)
    (cond ((not (list? expr)) (error "l0 ERROR: non-list instruction"))
          ((nil? expr) (error "l0 ERROR: nil instruction"))
          (else (case (first expr)
                  ((add) (cond ((nil? (drop expr 1)) (error "l0 ERROR: empty add instruction"))
                               ((not (nil? (drop expr 2))) (error "l0 ERROR: too many arguments in add instruction"))
                               ((number? (second expr)) (bf-number (second expr)))
                               (else (error "l0 ERROR: invalid add instruction argument type"))))
                  ((move) (cond ((nil? (drop expr 1)) (error "l0 ERROR: empty move instruction"))
                                ((not (nil? (drop expr 2))) (error "l0 ERROR: too many arguments in move instruction"))
                                ((number? (second expr)) (bf-move (second expr)))
                                (else (error "l0 ERROR: invalid move instruction argument type"))))
                  ((reset) (cond ((not (nil? (drop expr 1))) (error "l0 ERROR: too many arguments in reset instruction"))
                                 (else "[-]")))
                  ((input) (cond ((not (nil? (drop expr 1))) (error "l0 ERROR: too many arguments in input instruction"))
                                 (else ",")))
                  ((output) (cond ((not (nil? (drop expr 1))) (error "l0 ERROR: too many arguments in output instruction"))
                                  (else ".")))
                  ((while) (string-append "[" (compile (drop expr 1)) "]"))
                  (else (error "l0 ERROR: unknown instruction" (first expr)))))))

  (define (compile exprs)
    (apply string-append (map compile-one exprs)))

  (define (optimize exprs)
    (let loop ((left '((_)))
               (right exprs))
      (define (part position argument)
        (list-ref (list-ref right position) argument))
      (define (loop-stay new-right start)
        (loop left (append new-right (drop right start))))
      (define (loop-back new-right start)
        (loop (drop left 1) (append `(,(first left)) new-right (drop right start))))
      (cond ((nil? right)
             (drop (reverse left) 1))
            ((eq? (part 0 0) 'while)
             (loop `((while . ,(optimize (drop (first right) 1))) . ,left) (drop right 1)))
            ((and (eq? (part 0 0) 'add) (= (part 0 1) 0))
             (loop-back '() 1))
            ((and (eq? (part 0 0) 'move) (= (part 0 1) 0))
             (loop-back '() 1))
            ((nil? (drop right 1))
             (drop (reverse `(,(first right) . ,left)) 1))
            ((and (eq? (part 0 0) 'add) (eq? (part 1 0) 'add))
             (loop-stay `((add ,(+ (part 0 1) (part 1 1)))) 2))
            ((and (eq? (part 0 0) 'move) (eq? (part 1 0) 'move))
             (loop-stay `((move ,(+ (part 0 1) (part 1 1)))) 2))
            ((and (or (eq? (part 0 0) 'reset)
                      (eq? (part 0 0) 'add))
                  (or (eq? (part 1 0) 'reset)
                      (eq? (part 1 0) 'input)))
             (loop-back '() 1))
            (else
             (loop `(,(first right) . ,left) (drop right 1))))))

  `((compile . ,compile)
    (optimize . ,optimize)))

;; COMPILER

(define l0 (make-l0))
(define (compile expr)
  (let* ((expr ((cdr (assoc 'optimize l0)) expr))
         (expr ((cdr (assoc 'compile l0)) expr)))
    expr))

;; CLI

(define example '((add 1) (move 10) (move -10) (add -2) (while (input) (add 10) (reset) (input) (add -5) (output)) (input) (output) (reset) (reset)))
(display (compile example))
(newline)
