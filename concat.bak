#lang racket
(require flomat)


(define A (matrix '((1 2 3) (11 22 33)
                    (33 44 55))))
(for/list ([   x  (in-row A 0)]) x)

(define b (for/list ([   x  (in-row A 0)]) (+ x 1)))
(display b)
(for/list ([(i x) (in-row A 0)]) (list x i))
(display (cons b (sub A 1 0 1 3)))

(for/list ([   x  (in-col A 0)]) x)


(for/list ([(i x) (in-col A 0)]) (list x i))

(define insert_row (λ(matrix i rowi n)
      (let ((sub-top (sub A 0 0 i n)))
        (let ((sub-buttom (sub A (+ i 1) 0 (- n i) n)))
          (matrix (cons sub-top (cons rowi sub-buttom)))
        ))))

(define x (insert_row A 1 b 3))