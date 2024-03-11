# -*- coding: utf-8 -*-
"""
Created on Mon Mar 11 13:58:01 2024

The Matrix class encapsulates matrix operations including Gaussian elimination, determinant calculation, rank determination, and inverse matrix calculation.

"""

import numpy as np

class Matrix:
    def __init__(self, matrix):
        """
        Initialize the Matrix object with the input matrix.

        Parameters:
        matrix (numpy.ndarray): Input matrix of shape (n, n).
        """
        self.matrix = matrix
        self.echelon_form, self.scalar, self.flag, self.invereted_matrix = self.gaussian_elimination()

    def gaussian_elimination(self):
        """
        Function to perform Gaussian elimination of a matrix.

        Returns:
        Gaussian elimination of the input matrix.
        """
        row, col = 0, 0
        n = self.matrix.shape[0]
        res = np.copy(self.matrix).astype(float)
        flag = 1
        scalar = 1.0
        inverted = np.identity(n)
        while row < n and col < n:
            pivot_row = self.find_pivot_row(res, row, col)
            if pivot_row is not None:
                inverted[row], inverted[pivot_row] = inverted[pivot_row], inverted[row]
                res[row], res[pivot_row] = res[pivot_row], res[row]
                scalar /= res[row][col]
                inverted[row] /= res[row][col]
                res[row] /= res[row][col]
                if pivot_row != row:
                    flag *= -1
                for r in range(n):
                    if r != pivot_row:
                        factor = res[r, col]
                        inverted[r] -= factor * inverted[pivot_row]
                        res[r] -= factor * res[pivot_row]
                row += 1
            col += 1
        return res, scalar, flag, inverted

    def find_pivot_row(self, matrix, start_row, col):
        """
        Function to find the pivot row in a given column of the matrix.

        Returns:
        Index of the pivot row, or None if no pivot is found.
        """
        num_rows = matrix.shape[0]
        for row in range(start_row, num_rows):
            if matrix[row, col] != 0:
                return row
        return None

    def determinant(self):
        """
        Function to calculate the determinant of a matrix.

        Returns:
        Determinant of the input matrix.
        """
        m = self.echelon_form

        n = m.shape[0]
        det = 1
        for i in range(n):
            det *= m[i][i]
            if det == 0:
                return det
        if det != 0:
            det *= self.flag
            det /= self.scalar
        return det

    def rank(self):
        """
        Function to calculate the rank of a matrix.

        Returns:
        Rank of the input matrix.
        """

        rank_m = 0
        m = self.echelon_form
        n = m.shape[0]
        for row in range(n):
            for col in range(row, n):
                if m[row][col] != 0:
                    rank_m += 1
                    break
        return rank_m

    def inverse_matrix(self):
        """
        Function to calculate the inverse of a matrix.

        Parameters:
        Input matrix m of shape (n, n).

        Returns:
        Inverse of the input matrix.
        """
        if self.determinant() != 0:
            return self.invereted_matrix
        return None
matrix=Matrix( np.array([[1, 2, 3],
                   [4, 5, 6],[7,2,9]]))
print(matrix.rank()) 
print(matrix.determinant()) 
print(matrix.inverse_matrix()) 

#matrix= np.loadtxt('det_matrix(800 x 800).txt', usecols=range(800))