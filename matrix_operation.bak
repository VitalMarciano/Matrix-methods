#lang racket

;; Function to extract an element from a matrix given row i and column j
(define (extract matrix i j)
  "Extracts the element at row i and column j from the matrix."
  (vector-ref (vector-ref matrix i) j))

;; Function to swap two rows in a matrix
(define (swap-rows! matrix i j)
  "Swaps rows i and j in the matrix."
  (let ((tmp (vector-ref matrix i)))
    (vector-set! matrix i (vector-ref matrix j))
    (vector-set! matrix j tmp)))

;; Function to scale a row in a matrix by a given factor
(define (scale-row! matrix i factor)
  "Scales row i in the matrix by the given factor."
  (define (vector-scale vec factor)
    (vector-map (curry * factor) vec))
  (let* ((row (vector-ref matrix i))
         (scaled-row (vector-scale row factor)))
    (vector-set! matrix i scaled-row)))

;; Function to eliminate rows below a pivot element in the same column
(define (eliminate-rows! matrix i j pivot)
  "Eliminates rows below row i in the same column j using the pivot element."
  (define (eliminate-row! matrix i j pivot)
    (when (< i (vector-length matrix))
      (let* ((element-i-j (extract matrix i j))
             (factor (/ element-i-j pivot)))
        (scale-row! matrix i factor)
        (eliminate-row! matrix (add1 i) j pivot))))
  (eliminate-row! matrix (add1 i) j pivot))

;; Function to compute the rank of a matrix using Gaussian elimination
(define (gaussian-rank matrix (i 0) (j 0) (rank 0))
  "Computes the rank of the matrix using Gaussian elimination."
  (cond ((>= i (vector-length matrix)) rank)
        ((zero? (extract matrix i j)) (gaussian-rank matrix (add1 i) j rank))
        (else
          (swap-rows! matrix i rank)
          (let ((pivot (extract matrix rank j)))
            (when (not (zero? pivot))
              (scale-row! matrix rank (/ 1 pivot))
              (eliminate-rows! matrix rank j pivot))
            (gaussian-rank matrix (add1 i) (add1 j) (add1 rank))))))

;; Example matrix
(define mat (vector #(1 2 3) #(0 1 2) #(0 0 1)))

;; Compute the rank of the matrix
(display "Rank of the matrix: ")
(display (gaussian-rank mat)) ;;=> 1
