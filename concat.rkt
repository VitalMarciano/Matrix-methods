#lang racket
(require flomat)


(define A (matrix '((1 2 3) (11 22 33)
                    (33 44 55))))

(define insert_row (λ(mat i rowi n)
      (let ((sub-top (sub mat 0 0 i n)))
        (let ((sub-buttom (sub mat (+ i 1) 0 (-(- n i)1) n)))
           (stack sub-top (stack rowi sub-buttom))
        ))))
(define determinente (λ(mat det i)
                       (determinente(mat (* det ref(mat i i) (+ i 1))))))
(define c (transpose(matrix '[33 44 55])))
(define x (insert_row A 1 c 3))
(display x)
