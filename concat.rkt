#lang racket
(require flomat)

(define swap-rows(λ(mat rowi pivot-row n)
                       
                    (let((tmp1 (sub mat rowi 0 1 n)) (tmp2 (sub mat pivot-row 0 1 n)))
                         (begin
                         (insert_row mat rowi tmp2 n)
                         (insert_row mat pivot-row tmp1 n)
                         )
                     )))

(define A (matrix '((0 0 3) (1 11 22)(0 44 22))))
(for/list ([   x  (in-row A 0)]) x)


(define insert_row (λ(mat i rowi n)
           (cond
             ((= i 0) (stack rowi (sub mat (+ i 1) 0 (-(- n i)1) n)))
             ((= i (- n 1))(stack (sub mat 0 0 i n) rowi))
             (else (let((sub-top (sub mat 0 0 i n))
                        (sub-buttom (sub mat (+ i 1) 0 (-(- n i)1) n)))
                     (stack sub-top (stack rowi sub-buttom))))
                  )))

(define insert_col (λ(mat i coli n)
           (cond
             ((= i 0) (augment  coli (sub mat  0 (+ i 1) n (-(- n i)1))))
             ((= i (- n 1))(augment  (sub mat 0 0 n i) coli))
             (else (let((sub-top (sub mat 0 0 n i))
                        (sub-buttom (sub mat  0 (+ i 1) n (-(- n i)1))))
                     (augment  sub-top (augment  coli sub-buttom))))
                  )))



(define find-pivot(λ (mat start-row coli n)
                    (cond 
                          ((= start-row n) -1)
                          ((not (= (ref mat start-row coli) 0))
                          start-row)
                     (else (find-pivot mat (+ start-row 1) coli n)))))

(define gaussian-elimination (λ (mat i j n)
          ;1.stop recurstion
          ;(or (= i n) (= j n) (mat))
          ;2. no pivot col+=1
          ; ((= pivot -1 ) (gaussian-elimination mat i (+ j 1) n) )
          ;3.  row+=1 (defualt after all the operations sends with the new martix )
          ; ((= pivot -1 ) (gaussian-elimination mat (+ i 1) j n) )


                               
                               ;(cond (

                                      ;stop recurstion

                                   ;   (or (= i n) (= j n) (mat))
                              (begin
                               ; working
                               (define pivot (find-pivot mat i j n))
   
                                        ; pivot != row aka i
                                        (and (positive? pivot) (not(= i pivot))
                                             ;sum res[row]+=res[pivot_row]
                                            (insert_row mat i (.+ (sub mat i 0 1 n)(sub mat pivot 0 1 n)) n))
                                             ;add a function that substruct and do the multi matrix
                                             ;call her here
                              ;not working
                              ;   (let*
                              ;   (coli (./ (sub mat 0 j n 1) (ref mat i j)))
                              ;   (rowi (sub mat i 0 1 n))
                              ;   (multi-mat (outer coli rowi))
                              ;     )
                                        ; no pivot col+=1
                                        ;((= pivot -1 ) (gaussian-elimination mat i (+ j 1) n) )
                                        

                               )))
(define determinente (λ(mat det a i n)
                       (if (= i n) (* det  a)
                       (determinente mat (* det (ref mat i i)) a (+ i 1) n))))


(define rank (λ(mat n i r)
               (cond ((= i n) r)
                   ((not(= (ref mat i i) 0))(rank mat n (+ i 1) (+ r 1))))))


(display A)
(define c (transpose(matrix '[0 44 78])))
(define v (matrix '[0 44 78]))
(define x (gaussian-elimination A 0 0 3))
(display x)
(define d (determinente A 1 -1 0 3))
(display d)

(define colA (sub A 0 0 3 1))
(define rowA (sub A 0 0 1 3))

(display "\n")
(display (outer colA rowA))
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