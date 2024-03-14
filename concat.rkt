#lang racket
(require flomat)



(define A (matrix '((0 0 3) (1 11 22)(0 44 22))))
(for/list ([   x  (in-row A 0)]) x)


(define insert-row (λ(mat i rowi n)
           (cond
             ((= i 0) (stack rowi (sub mat (+ i 1) 0 (-(- n i)1) n)))
             ((= i (- n 1))(stack (sub mat 0 0 i n) rowi))
             (else (let((sub-top (sub mat 0 0 i n))
                        (sub-buttom (sub mat (+ i 1) 0 (-(- n i)1) n)))
                     (stack sub-top (stack rowi sub-buttom))))
                  )))

(define insert-col (λ(mat i coli n)
           (cond
             ((= i 0) (augment  coli (sub mat  0 (+ i 1) n (-(- n i)1))))
             ((= i (- n 1))(augment  (sub mat 0 0 n i) coli))
             (else (let((sub-top (sub mat 0 0 n i))
                        (sub-buttom (sub mat  0 (+ i 1) n (-(- n i)1))))
                     (augment  sub-top (augment  coli sub-buttom))))
                  )))


(define eliminate (λ(mat i j pivot n)
                    " get mat, i (row index), j (col index) and n
                      return row i * (col j)/ matrix[i][j]"
                (let*                    
                    (
                     (coli (mset! ( ./ (sub mat 0 j n 1) pivot ) i 0 0))     
                     (rowi (sub mat i 0 1 n))
                     (mult-mat (outer coli rowi))
                     (res-mat (.- mat mult-mat))
                  
                     )
                  
                    (insert-row res-mat i (./ rowi pivot) n))
                  ))


(define find-pivot(λ (mat start-row coli n)
                    (cond

                      ((= start-row n) -1)
                      ((not (= (ref mat start-row coli) 0))
                          start-row)
                     (else (find-pivot mat (+ start-row 1) coli n)))))

(define gaussian-elimination (λ (mat invers i j n)
          "1.stop recurstion
          (or (= i n) (= j n) (mat))
          2. no pivot col+=1
           ((= pivot -1 ) (gaussian-elimination mat i (+ j 1) n) )
          3.  row+=1 (defualt after all the operations sends with the new martix )
           ((= pivot -1 ) (gaussian-elimination mat (+ i 1) (+ j 1) n) )"
                               ;stop recurstion
                                 (if (or (= i n) (= j n)) invers
                                  (let* ((pivot (find-pivot mat i j n)))
                                    ; pivot != row aka i: sum res[row]+=res[pivot_row]
                                    (cond
                                      ((= pivot -1)
                                        (gaussian-elimination mat invers i (+ j 1) n))
                                      (( not (= i pivot))
                                       (let* (
                                              (ri (.+ (sub invers i 0 1 n)(sub mat pivot 0 1 n)))
                                              (mi  (insert-row invers i ri n))
                                              (r (.+ (sub mat i 0 1 n)(sub mat pivot 0 1 n)))
                                              (m  (insert-row mat i r n))
                                              (tm1i (eliminate mi i j (ref m i j) n))
                                              (tm1 (eliminate m i j (ref m i j) n)))
                                         (gaussian-elimination tm1 tm1i (+ i 1) (+ j 1) n)))
                                      (else (let* ((tm2i (eliminate invers i j (ref mat i j) n))
                                                   (tm2 (eliminate mat i j (ref mat i j) n)))
                                              (gaussian-elimination tm2 tm2i (+ i 1) (+ j 1) n)))) ))))
                                       
                                       

(define determinente (λ(mat det a i n)
                       (if (= i n) (* det  a)
                       (determinente mat (* det (ref mat i i)) a (+ i 1) n))))


(define rank (λ(mat n i r)
               (cond ((= i n) r)
                   ((not(= (ref mat i i) 0))(rank mat n (+ i 1) (+ r 1))))))


(display A)
(define I (eye 3 3))
(define x (gaussian-elimination A I 0 0 3))

(display "\ngaussian-elimination\n")
(display x)
(display "\n")
(define d (determinente A 1 -1 0 3))
(display d)
(display (inv A)) 


;read from a file  the matrix
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