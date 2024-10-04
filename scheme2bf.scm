(use-modules (srfi srfi-1))

;; L0: SIMPLE BYTE TAPE
;;
;; move n        -- Move right n bytes. If negative, move left.
;; add n         -- Add n to current byte.
;; reset         -- Reset current byte to 0.
;; input         -- Input to current byte.
;; output        -- Output current byte.
;; while . exprs -- Loop expressions while current byte is not 0.

(define (make-l0 port)
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
    (cond ((not (list? expr)) (error))
          ((nil? expr) (error))
          (else (case (first expr)
                  ((add) (cond ((nil? (drop expr 1)) (error))
                               ((not (nil? (drop expr 2))) (error))
                               ((number? (second expr)) (display (bf-number (second expr)) port))
                               (else (error))))
                  ((move) (cond ((nil? (drop expr 1)) (error))
                                ((not (nil? (drop expr 2))) (error))
                                ((number? (second expr)) (display (bf-move (second expr)) port))
                                (else (error))))
                  ((reset) (cond ((not (nil? (drop expr 1))) (error))
                                 (else (display "[-]" port))))
                  ((input) (cond ((not (nil? (drop expr 1))) (error))
                                 (else (display "," port))))
                  ((output) (cond ((not (nil? (drop expr 1))) (error))
                                  (else (display "." port))))
                  ((while) (display "[" port) (compile (drop expr 1)) (display "]" port))
                  (else (error))))))

  (define (compile exprs)
    (for-each compile-one exprs))

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

;; L1: LAYERED TAGGED BYTE TAPE
;; Cell: [byte]< [tag]
;; [L1_1] [L2_1] ... [L1_2] [L2_2] ... ...
;;
;; move n               -- Move right n cells in the current layer. If negative, move left.
;; add n                -- Add n to current cell.
;; reset                -- Reset current cell to 0.
;; input                -- Input to current cell.
;; output               -- Output current cell.
;; tag-from-untagged    -- Make current untagged cell tagged.
;; untag-from-tagged    -- Make current tagged cell untagged.
;; tag                  -- Make current cell tagged.
;; untag                -- Make current cell untagged.
;; find-tagged-left     -- Find nearest tagged cell on left in current layer.
;; find-tagged-right    -- Find nearest tagged cell on right in current layer.
;; find-untagged-left   -- Find nearest untagged cell on left in current layer.
;; find-untagged-right  -- Find nearest untagged cell on right in current layer.
;; while-value . exprs  -- Loop expressions while current cell is not 0.
;; while-tagged . exprs -- Loop expressions while current cell is tagged.
;; copy-take            -- Take one from cell for copying. Assumes untagged.
;; copy-put             -- Put one at cell for copying.
;; copy-reset           -- Fix cell after copying.
;; next-layer           -- Switch to the next layer.
;; prev-layer           -- Switch to the previous layer.

(define (make-l1 layers)
  (define (compile-one expr)
    (cond ((not (list? expr)) (error))
          ((nil? expr) (error))
          (else (case (first expr)
                  ((add) (cond ((nil? (drop expr 1)) (error))
                               ((not (nil? (drop expr 2))) (error))
                               ((number? (second expr)) `((add ,(second expr))))
                               (else (error))))
                  ((move) (cond ((nil? (drop expr 1)) (error))
                                ((not (nil? (drop expr 2))) (error))
                                ((number? (second expr)) `((move ,(* 2 layers (second expr)))))
                                (else (error))))
                  ((reset) (cond ((not (nil? (drop expr 1))) (error))
                                 (else '((reset)))))
                  ((input) (cond ((not (nil? (drop expr 1))) (error))
                                 (else '((input)))))
                  ((output) (cond ((not (nil? (drop expr 1))) (error))
                                  (else '((output)))))
                  ((tag-from-untagged) (cond ((not (nil? (drop expr 1))) (error))
                                             (else '((move 1) (add 1) (move -1)))))
                  ((untag-from-tagged) (cond ((not (nil? (drop expr 1))) (error))
                                             (else '((move 1) (add -1) (move -1)))))
                  ((tag) (cond ((not (nil? (drop expr 1))) (error))
                               (else '((move 1) (reset) (add 1) (move -1)))))
                  ((untag) (cond ((not (nil? (drop expr 1))) (error))
                                 (else '((move 1) (reset) (move -1)))))
                  ((find-tagged-left) (cond ((not (nil? (drop expr 1))) (error))
                                            (else '((move ,(1 - (* 2 layers))) (add -1)
                                                    (while (add 1) (move ,(* -2 layers)) (add -1)) (add 1) (move -1)))))
                  ((find-tagged-right) (cond ((not (nil? (drop expr 1))) (error))
                                             (else '((move ,(1 + (* 2 layers))) (add -1)
                                                     (while (add 1) (move ,(* 2 layers)) (add -1)) (add 1) (move -1)))))
                  ((find-untagged-left) (cond ((not (nil? (drop expr 1))) (error))
                                              (else '((move ,(1 - (* 2 layers))) (while (move ,(* -2 layers))) (move -1)))))
                  ((find-untagged-right) (cond ((not (nil? (drop expr 1))) (error))
                                               (else '((move ,(1 + (* 2 layers))) (while (move ,(* -2 layers))) (move -1)))))
                  ((while-value) `((while . ,(compile (drop expr 1)))))
                  ((while-tagged) `((move 1) (while (move -1) ,@(compile (drop expr 1)) (move 1)) (move -1)))
                  ((copy-take) (cond ((not (nil? (drop expr 1))) (error))
                                     (else '((add -1) (move 1) (add 1) (move -1)))))
                  ((copy-put) (cond ((not (nil? (drop expr 1))) (error))
                                    (else '((add 1)))))
                  ((copy-reset) (cond ((not (nil? (drop expr 1))) (error))
                                      (else '((move 1) (while (add -1) (move -1) (add 1) (move 1)) (move -1)))))
                  ((next-layer) (cond ((not (nil? (drop expr 1))) (error))
                                      (else '((move 2)))))
                  ((prev-layer) (cond ((not (nil? (drop expr 1))) (error))
                                      (else '((move -2)))))
                  (else (error))))))

  (define (compile exprs)
    (apply append (map compile-one exprs)))

  (define (optimize exprs)
    exprs)

  `((compile . ,compile)
    (optimize . ,optimize)))

;; COMPILER

(define l0 (make-l0 (current-output-port)))
(define l1 (make-l1 2))
(define (compile expr)
  (let* ((expr ((cdr (assoc 'optimize l1)) expr))
         (expr ((cdr (assoc 'compile l1)) expr))
         (expr ((cdr (assoc 'optimize l0)) expr))
         (expr ((cdr (assoc 'compile l0)) expr)))
    #t))

;; CLI

(define example '((add 10)
                  (while-value (copy-take) (move 1) (copy-put) (move -1))
                  (copy-reset)
                  (next-layer)
                  (add 10)
                  (while-value (copy-take) (move 1) (copy-put) (move -1))
                  (copy-reset)
                  (prev-layer)))
(compile example)
(newline)
