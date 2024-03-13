#lang racket
(require flomat)


(define A (matrix '((1 2 3) (11 22 33)
                    (33 44 55))))

(define insert_row (λ(mat i rowi n)
      (let ((sub-top (sub mat 0 0 i n)))
        (let ((sub-buttom (sub mat (+ i 1) 0 (-(- n i)1) n)))
           (stack sub-top (stack rowi sub-buttom))
        ))))
(define determinente (λ(mat det a i n)
                       ((= i n) (* det  a))
                       (determinente(mat (* det (ref mat i i) a (+ i 1) n)))))
(define c (transpose(matrix '[33 44 55])))
(define x (insert_row A 1 c 3))
(display x)
(define d (determinente A 1 -1 0 3))
(display d)
(define path "det_matrix(800 x 800).txt")
(define delim #rx"([ ]*(,)[ ]*)|([ ]+)")

(define (next-line-it file)
  (let ((line (read-line file 'any)))
    (if (eof-object? line)
        (list)
        (cons line (next-line-it file)))))

;Function to read a matrix from text file
(define read_data (λ(file delim)
      (define file (open-input-file path))
      (define lines (call-with-input-file path next-line-it))
      (define numbers (map (λ(x) (string-split x delim)) lines))
      (define mat (map (λ(x)(map string->number x)) numbers))
      (close-input-port file)
       mat))
(define t_start (current-inexact-milliseconds)) ;Get current time
(define m (read_data path delim))
(define t_end (current-inexact-milliseconds))