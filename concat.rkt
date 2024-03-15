#lang racket
(require flomat)


(define insert-row (λ(mat i rowi n)
           (cond
             ((= i 0) (stack rowi (sub mat (+ i 1) 0 (-(- n i)1) n)))
             ((= i (- n 1))(stack (sub mat 0 0 i n) rowi))
             (else (let((sub-top (sub mat 0 0 i n))
                        (sub-buttom (sub mat (+ i 1) 0 (-(- n i)1) n)))
                     (stack sub-top (stack rowi sub-buttom))))
                  )))

(define eliminate (λ(mat i j col-mat pivot n)
                    " get mat, i (row index), j (col index) and n
                      return row i * (col j)/ matrix[i][j]"
                (let*                    
                    (
                     (coli (mset! ( ./ col-mat pivot ) i 0 0))     
                     (rowi (sub mat i 0 1 n))
                     (mult-mat (outer coli rowi))
                     (res-mat (.- mat mult-mat))
                  
                     )
                  
                    (insert-row res-mat i (./ rowi pivot) n))
                  ))
(define eliminate-rows (λ(mat i j pivot n)
                    " get mat, i (row index), j (col index) and n
                      return row i * (col j)/ matrix[i][j]"
                (let*                    
                    (
                     (coli ( ./ (sub mat 0 j n 1) pivot ))
                     (col-s (sub! coli 0 0 ( + i 1) 1))
                     (rowi (sub mat i 0 1 n))
                     )
                  (zeros! col-s)
                  (.- mat  (outer coli rowi)) 
                  )))


(define find-pivot(λ (mat start-row coli n)
                    (cond

                      ((= start-row n) -1)
                      ((not (= (ref mat start-row coli) 0))
                          start-row)
                     (else (find-pivot mat (+ start-row 1) coli n)))))

(define inverse-matrix-calculation (λ (mat invers i j n)
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
                                        (inverse-matrix-calculation mat invers i (+ j 1) n))
                                      (( not (= i pivot))
                                       (let* (
                                              (ri (.+ (sub invers i 0 1 n)(sub invers pivot 0 1 n)))
                                              (mi  (insert-row invers i ri n))
                                              (r (.+ (sub mat i 0 1 n)(sub mat pivot 0 1 n)))
                                              (m  (insert-row mat i r n))
                                              (coli (sub m 0 j n 1))
                                              (tm1i (eliminate mi i j coli (ref m i j) n))
                                              (tm1 (eliminate m i j coli (ref m i j) n)))
                                         (inverse-matrix-calculation tm1 tm1i (+ i 1) (+ j 1) n)))
                                      (else (let* ((tm2i (eliminate invers i j (sub mat 0 j n 1) (ref mat i j) n))
                                                   (tm2 (eliminate mat i j (sub mat 0 j n 1) (ref mat i j) n)))
                                              (inverse-matrix-calculation tm2 tm2i (+ i 1) (+ j 1) n)))) ))))
                                       
                                       
(define gaussian-elimination (λ (mat i j n)
                             (if (or (= i n) (= j n)) mat
                                 (let* ((pivot (find-pivot mat i j n)))
                                  (cond
                                      ((= pivot -1)
                                        (gaussian-elimination mat  i (+ j 1) n))
                                      (( not (= i pivot))
                                        (let* (
                                              (row-sum (.+ (sub mat i 0 1 n)(sub mat pivot 0 1 n)))
                                              (m  (insert-row mat i row-sum n))
                                              (m1 (eliminate-rows m i j (ref m i j) n))
                                              )
                                          (gaussian-elimination m1  (+ i 1)  (+ j 1) n))
                                 )
                                      (else (let* (
                                                   (m (eliminate-rows mat i j (ref mat i j) n)))
                                              (gaussian-elimination m (+ i 1) (+ j 1) n))))
                               )))
                               )

(define determinente-calculation (λ(mat det i n)
                       
                       (if (>= i n)  det  
                       (determinente-calculation mat (* det (ref mat i i)) (+ i 1) n))))

(define determinente (λ(mat)
                      (let (
                            (ranked (gaussian-elimination mat 0 0 (nrows mat))
                                  ))(determinente-calculation ranked 1 0 (nrows mat) ))))
(define scan-row(λ( mat row j n)
               (cond(
                     (= j n) 0)
                     ((not(= (ref mat row j) 0)) 1)
                  (scan-row mat row (+ j 1) n))))

(define rank-calculation (λ(mat n i  r)
               (let ((matrix(gaussian-elimination mat 0 0 n)))
               (cond ((= i n) r)
                   ((= (scan-row matrix i i n) 1)
                    (rank-calculation matrix n (+ i 1) (+ r 1)))
                    ))))

(define rank (λ(mat)
               (let(
                    (ranked (gaussian-elimination mat 0 0 (nrows mat))  ))
                 (rank-calculation ranked (nrows mat) 0 0))))

(define inverse-matrix (λ(mat)
                        (inverse-matrix-calculation mat (eye (nrows mat)) 0 0 (nrows mat))))


(define A (matrix '((1 2 3) (4 5 6) (7 2 9))))
(display A)
(define I (eye 3))
(define x (inverse-matrix A))

(display "\ngaussian-elimination\n")
(display x)
(display "\n")
(define r (rank A))
(display "\n rank\n")
(display r)
(define d (determinente A))
(display "\n determinente\n")
(display d)
(display (inv A)) 


;read from a file  the matrix
(define path_det "det_matrix(800 x 800).txt")
(define path_inverse "inv_eig_matrix(800 x 800).txt")
(define path_rank "rank_matrix(1000x1000).txt")
(define delim #rx"([ ]*(,)[ ]*)|([ ]+)")

(define (next-line-it file)
  (let ((line (read-line file 'any)))
    (if (eof-object? line)
        (list)
        (cons line (next-line-it file)))))

;Function to read a matrix from text file
(define read_data (λ( path delim)
      (let(( file (open-input-file path)))
      (define lines (call-with-input-file path next-line-it))
      (define numbers (map (λ(x) (string-split x delim)) lines))
      (define mat (map (λ(x)(map string->number x)) numbers))
      (close-input-port file)
       mat)))
(define m2 (matrix  (read_data path_rank delim)))
(define t_start2 (current-inexact-milliseconds)) ;Get current time
(define ra (rank  m2))
(display ra)
(define t_end2 (current-inexact-milliseconds))
(display "\n rank ")
(display (- t_end2 t_start2) )

(define m (matrix  (read_data path_det delim)))
(define t_start (current-inexact-milliseconds)) ;Get current time
(define q (determinente  m))
(display q)
(define t_end (current-inexact-milliseconds))
(display "\n det ")
(display (- t_end t_start) )

(define m1 (matrix  (read_data path_inverse delim)))
(define t_start1 (current-inexact-milliseconds)) ;Get current time
(define i (inverse-matrix  m1))
(display i)
(define t_end1 (current-inexact-milliseconds))
(display "\n inverse ")
(display (- t_end1 t_start1) )

