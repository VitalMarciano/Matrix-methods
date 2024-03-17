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
        self.rank=0


    def inverse_matrix(self):
        """
        Function to calculate the inverse of a matrix.

        Parameters:
        Input matrix m of shape (n, n).

        Returns:
        Inverse of the input matrix.
        """
        row, col = 0, 0
        n = (self.matrix).shape[0]
        res = np.copy(self.matrix).astype(float)
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
                inverted[row] /= res[row][col]
                res[row] /= res[row][col]
                row+=1
            else:
                return "Not inversable"
            col+=1
        return inverted
  
    
    def gaussian_elimination(self):
        """
        Function to perform Gaussian elimination of a matrix.

        Returns:
        Gaussian elimination of the input matrix.
        """
        row, col = 0, 0
        n = (self.matrix).shape[0]
        res = np.copy(self.matrix).astype(float)
        pivot_row=0
        while pivot_row < n and col < n:
            if self.matrix[row][col]==0:
                pivot_row = self.find_pivot_row(res, row, col)
            else: 
                pivot_row=row
            if pivot_row!=None :
                # swap the rows
                self.rank+=1
                if pivot_row !=row :
                    res[row]+=res[pivot_row]
                     
                vec_col=np.copy(res[row+1:,col].reshape(-1,1))
                vec_col=vec_col/res[row,col]
                vec_row=np.copy(res[row]).reshape(1,-1)
                mat_multiply=vec_row*vec_col
                res[row+1:,:]-=mat_multiply 
                
                row+=1
            col+=1
        return res 
    
    
    def find_pivot_row(self,mat, start_row, col):
        """
        Function to find the pivot row in a given column of the matrix.

        Returns:
        Index of the pivot row, or None if no pivot is found.
        """
        num_rows = (self.matrix).shape[0]
        for row in range(start_row, num_rows):
            if mat[row, col] != 0:
                return row
        return None

    def determinant(self):
        """
        Function to calculate the determinant of a matrix.

        Returns:
        Determinant of the input matrix.
        """
        m = self.gaussian_elimination()
        det= np.prod(np.diag(m))
        return det

    def mat_rank(self):
        """
        Function to calculate the rank of a matrix.

        Returns:
        Rank of the input matrix.
        """
        self.gaussian_elimination()
        return self.rank

    
      

print("det_matrix(800 x 800).txt") 
matrix=np.loadtxt('det_matrix(800 x 800).txt', usecols=range(800))
matrix= Matrix(matrix)
t=time()
print(matrix.determinant()) 
print("TIME:",time()-t) 

print("python")
t=time()
print(np.linalg.det(matrix.matrix))
print("TIME:",time()-t) 

print("\nrank_matrix(1000x1000).txt")
matrix = np.loadtxt('rank_matrix(1000x1000).txt', delimiter=',', usecols=range(1000))
matrix= Matrix(matrix)
t=time()
print(matrix.mat_rank()) 
print("TIME:",time()-t) 

print("python")
t=time()
print(np.linalg.matrix_rank(matrix.matrix))
print("TIME:",time()-t)

print("\ninv_eig_matrix(800 x 800).txt")      
matrix= np.loadtxt('inv_eig_matrix(800 x 800).txt', usecols=range(800))
matrix= Matrix(matrix)
t=time()
print(matrix.inverse_matrix())
print("TIME:",time()-t) 

print("python")
t=time()
print(np.linalg.inv(matrix.matrix))
print("TIME:",time()-t) 