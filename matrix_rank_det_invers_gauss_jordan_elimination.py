# -*- coding: utf-8 -*-
"""
Created on Mon Mar 11 13:58:01 2024

The Matrix class encapsulates matrix operations including Gaussian elimination, determinant calculation, rank determination, and inverse matrix calculation.

"""

import numpy as np
from time import time

class Matrix:
    def __init__(self, matrix):
        """
        Initialize the Matrix object with the input matrix.

        Parameters:
        matrix (numpy.ndarray): Input matrix of shape (n, n).
        """
        self.matrix = matrix
        self.echelon_form, self.scalar, self.invereted_matrix = self.gaussian_elimination()

    def gaussian_elimination(self):
        """
        Function to perform Gaussian elimination of a matrix.

        Returns:
        Gaussian elimination of the input matrix.
        """
        row, col = 0, 0
        n = (self.matrix).shape[0]
        res = np.copy(self.matrix).astype(float)
        scalar=1.0
        inverted= np.identity(n)
        while row < n and col < n:
            
            pivot_row = self.find_pivot_row(res, row, col)
            if pivot_row!=None :
                # swap the rows
                if pivot_row !=row :
                    inverted[row]+=inverted[pivot_row]
                    res[row]+=res[pivot_row]
                     
                vec_col=np.copy(res[:,col].reshape(-1,1))
                vec_col[row][0]=0
               
                # The col is the same as the res matrix
                i_vec_col=np.copy(vec_col)
                vec_col=vec_col/res[row,col]
                i_vec_col=i_vec_col/res[row,col]
                vec_row=np.copy(res[row]).reshape(1,-1)
                # row as the identity matrix
                i_vec_row=np.copy(inverted[row]).reshape(1,-1)
                i_mat_multiply= i_vec_col * i_vec_row
                mat_multiply=vec_row*vec_col
                inverted-=i_mat_multiply
                res-=mat_multiply
    
                # divide the rows elements with the first nonzero element 
                scalar /=res[row][col]
                inverted[row] /= res[row][col]
                res[row] /= res[row][col]
               
    
                # Reset all other elements in the current column
                # for r in  [ 0.       range(n):
                #       if r!= pivot_row:
                #           factor = res[r, col]
                #           inverted[r] -= factor * inverted[pivot_row]
                #           res[r] -= factor * res[pivot_row]
                        
                row+=1
            col+=1
           
          
        print(res)
        return (res,scalar,inverted)  

    def find_pivot_row(self, matrix, start_row, col):
        """
        Function to find the pivot row in a given column of the matrix.

        Returns:
        Index of the pivot row, or None if no pivot is found.
        """
        num_rows = (self.matrix).shape[0]
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
        #if self.determinant() != 0:
        return self.invereted_matrix
        #return None
matrix=Matrix( np.array([[0, 3],[1,11]]))
#matrix=np.loadtxt('det_matrix(800 x 800).txt', usecols=range(800))
t=time()
#matrix= Matrix(matrix)
# print(matrix.rank()) 
print(matrix.determinant()) 
print(matrix.inverse_matrix())
print("TIME:",time()-t) 

print("python")
t=time()
print(np.linalg.inv(matrix.matrix))
print("TIME:",time()-t) 
