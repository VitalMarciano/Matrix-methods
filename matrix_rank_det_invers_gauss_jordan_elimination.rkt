#lang racket
(require flomat)


(define insert-row (λ(Mat i rowi n)
                     ; Insert a row into a matrix at a specified index
           (cond
             ((= i 0) (stack rowi (sub Mat (+ i 1) 0 (-(- n i)1) n)))
             ((= i (- n 1))(stack (sub Mat 0 0 i n) rowi))
             (else (let((sub-top (sub Mat 0 0 i n))
                        (sub-buttom (sub Mat (+ i 1) 0 (-(- n i)1) n)))
                     (stack sub-top (stack rowi sub-buttom))))
                  )))

(define eliminate (λ(Mat i j col-mat pivot n)
                    ; Perform row elimination operation to create zeros above and below the pivot element
                (let*                    
                    (
                     (V_col (mset! ( ./ col-mat pivot ) i 0 0))     
                     (V_row (sub Mat i 0 1 n))
                     (M_mult (outer V_col V_row))
                     (M_res (.- Mat M_mult))
                     )
                    (insert-row M_res i (./ V_row pivot) n));divide by the pivot element
                  ))
(define eliminate-rows (λ(Mat i j pivot n)
                         ; Perform row elimination operation to create zeros below the pivot element
                (let*                    
                    (
                     (V_col ( ./ (sub Mat 0 j n 1) pivot ))
                     (col-s (sub! V_col 0 0 ( + i 1) 1))
                     (V_row (sub Mat i 0 1 n))
                     )
                  (zeros! col-s)
                  (.- Mat  (outer V_col V_row)) 
                  )))


(define find-pivot(λ (Mat start-row col n)
                     ; Find the pivot element in the matrix for Gaussian elimination
                    (cond
                      ((= start-row n) -1); If we've reached the end of the matrix, return -1
                      ((not (= (ref Mat start-row col) 0)); If element is not zero, it's a pivot
                          start-row)
                     (else (find-pivot Mat (+ start-row 1) col n))))); Otherwise, search in the next row

(define inverse-matrix-calculation (λ (Mat Inverted_mat i j n)
                                     ; Calculate the inverse of a matrix using Gaussian elimination
                              
                                 (if (or (= i n) (= j n)) Inverted_mat ; Base case: stop recursion when we've traversed the entire matrix
                                  (let* ((pivot (find-pivot Mat i j n)))
                                    
                                    (cond
                                      ((= pivot -1) ; If pivot not found, move to the next column
                                        "Not inversable")
                                      (( not (= i pivot))
                                       (let* (
                                              (row_sum_i (.+ (sub Inverted_mat i 0 1 n)(sub Inverted_mat pivot 0 1 n)))
                                              (Mi  (insert-row Inverted_mat i row_sum_i n))
                                              (row_sum (.+ (sub Mat i 0 1 n)(sub Mat pivot 0 1 n)))
                                              (M_tmp  (insert-row Mat i row_sum n))
                                              (V-col (sub M_tmp 0 j n 1))
                                              (M_inv (eliminate Mi i j V-col (ref M_tmp i j) n))
                                              (M (eliminate M_tmp i j V-col (ref M_tmp i j) n)))
                                         (inverse-matrix-calculation M M_inv (+ i 1) (+ j 1) n)))
                                      (else ; If current row is pivot row
                                       (let* ((M_inv (eliminate Inverted_mat i j (sub Mat 0 j n 1) (ref Mat i j) n))
                                                   (M (eliminate Mat i j (sub Mat 0 j n 1) (ref Mat i j) n)))
                                              (inverse-matrix-calculation M M_inv (+ i 1) (+ j 1) n)))) ))))
                                       
                                       
(define gaussian-elimination (λ (Mat i j n rank-count det flag)
                               ; Perform Gaussian elimination on a matrix to compute its rank and determinant
                             (if (or (= i n) (= j n))
                                 (if(zero? flag)
                                  rank-count; Return rank if flag is 0
                                                       det); Return determinant if flag is 1
                                 (let* ((pivot (find-pivot Mat i j n)))
                                  (cond
                                      ((= pivot -1); If pivot not found, move to the next column
                                        (gaussian-elimination Mat  i (+ j 1) n rank-count 0 flag))
                                      (( not (= i pivot)) ; If current row is not pivot row
                                        (let* (
                                              (row-sum (.+ (sub Mat i 0 1 n)(sub Mat pivot 0 1 n)))
                                              (M  (insert-row Mat i row-sum n))
                                              (M-elim (eliminate-rows M i j (ref M i j) n))
                                              )
                                          (gaussian-elimination M-elim  (+ i 1)  (+ j 1) n (+ rank-count 1) (* det (ref M-elim i j)) flag )) )
                                      (else ; If current row is pivot row
                                       (let* (
                                                   (M (eliminate-rows Mat i j (ref Mat i j) n)))
                                              (gaussian-elimination M (+ i 1) (+ j 1) n (+ rank-count 1) (* det (ref M i j)) flag )))) ))))




(define determinente (λ(Mat)
                         ; Compute the determinant of a matrix using Gaussian elimination
                       (gaussian-elimination Mat 0 0 (nrows Mat) 0 1 1 )))

(define rankM (λ(Mat)
               ; Compute the rank of a matrix using Gaussian elimination
           (gaussian-elimination Mat 0 0 (nrows Mat) 0 1 0)  ))
               
(define inverse-matrix (λ(Mat)
                           ; Compute the inverted of a matrix using Gaussian elimination
                        (inverse-matrix-calculation Mat (eye (nrows Mat)) 0 0 (nrows Mat))))
(define A (matrix '((0 0 3) (0 11 22)(0 44 22))))
(display (determinente A))
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
(define ra (rankM  m2))
(display "\n rank:  ")
(display ra)
(define t_end2 (current-inexact-milliseconds))
(display "\n rank time: ")
(display (- t_end2 t_start2) )

(define m (matrix  (read_data path_det delim)))
(define t_start (current-inexact-milliseconds)) ;Get current time
(define q (determinente  m))
(display "\n det: ")
(display q)
(define t_end (current-inexact-milliseconds))
(display "\n det time: ")
(display (- t_end t_start) )

(define m1 (matrix  (read_data path_inverse delim)))
(define t_start1 (current-inexact-milliseconds)) ;Get current time
(define i (inverse-matrix  m1))
(display "\n inverse: ")
(display i)
(define t_end1 (current-inexact-milliseconds))
(display "\n inverse time: ")
(display (- t_end1 t_start1) )
(display "\n SCHEME \n  ")

(define t_startS (current-inexact-milliseconds)) ;Get current time
(define ds (det  m))
(display "\n det: ")
(display ds)
(define t_endS (current-inexact-milliseconds))
(display "\n det time: ")
(display (- t_endS t_startS) )

(define t_start11 (current-inexact-milliseconds)) ;Get current time
(define i1 (inv  m1))
(display "\n inverse: ")
(display i)
(define t_end11 (current-inexact-milliseconds))
(display "\n inverse time: ")
(display (- t_end11 t_start11) )
(define t_start2S (current-inexact-milliseconds)) ;Get current time
(define ra1 (rank  m2))
(display "\n rank:  ")
(display ra1)
(define t_end2S (current-inexact-milliseconds))
(display "\n rank time: ")
(display (- t_end2S t_start2S) )