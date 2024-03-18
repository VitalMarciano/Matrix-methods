# Matrix Operations in Python and Racket
This project implements various matrix operations, including determinant calculation, rank determination, and inverse matrix calculation, using Gaussian elimination or Gauss-Jordan elimination. Implementations are provided in both Python and Racket.

# Features:

Determinant calculation: Calculates the determinant of a square matrix.
Rank determination: Calculates the rank of a matrix.
Inverse matrix calculation: Computes the inverse of a square matrix (if it exists).
Gaussian/Gauss-Jordan elimination: Implements these elimination algorithms for matrix calculations.
Usage:

# Python:

Install NumPy: pip install numpy
Import the Matrix class from matrix.py.
Create a matrix object using a NumPy array.
Use the provided methods like determinant(), mat_rank(), and inverse_matrix() on the matrix object.
# Racket:
The Racket implementation is likely in matrix_rank_det_invers_gauss_jordan_elimination.rkt
(Specific usage instructions might be within the file itself).
used [flomat library] (https://docs.racket-lang.org/manual-flomat/index.html)
# Data Files:
det_matrix(800 x 800).txt, inv_eig_matrix(800 x 800).txt, and rank_matrix(1000x1000).txt contain test data for matrices.
# Performance Considerations:
The provided Python code offers custom implementations, but for performance-critical tasks, consider using optimized functions from the numpy.linalg library.

# Future Improvements:

Unit tests to ensure the correctness of the implemented functions in both languages.
Error handling for non-square matrices or cases where the inverse doesn't exist.
Documentation within the code for better readability.
#  Getting Started:

For detailed usage instructions and potential language-specific requirements, refer to the respective Python and Racket files (matrix.py and matrix_rank_det_invers_gauss_jordan_elimination.rkt).

This project provides a starting point for exploring matrix operations with Gaussian elimination/Gauss-Jordan elimination in Python and Racket. Feel free to modify and extend the code based on your specific needs.
# Authors

* Vital Marciano
* Neta Amzalag
