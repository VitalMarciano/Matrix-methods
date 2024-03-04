
from time import time
import numpy as np


def gaussian_elimination(m):
    """
    Function to preform the gaussian elimination of a matrix.

    Parameters:
    Input matrix m of shape (n, n).

    Returns:
    Gaussian elimination of the input matrix.
    
    """
    row, col = 0, 0
    n = m.shape[0]
    res = np.copy(m) 
    while row < n and col < n:
        pivot_row = row
        if pivot_row:
            # swap the rows
            res[row], res[pivot_row] = res[pivot_row], res[row]
            # divide the rows elements with the first nonzero element 
            res[row] /= res[row][col]
            # Reset all other elements in the current column
            for r in range(n):
                if r!= pivot_row:
                    factor = res[r, col]
                    res[r] -= factor * res[pivot_row]
            row+=1
        col+=1
    return res    
    
    


def inverse_matrix(m):
   """
   Function to calculate the inverse of a matrix.

   Parameters:
   Input matrix m of shape (n, n).

   Returns:
   Inverse of the input matrix.
   
   """
   
def determinant(m):
    """
    Function to calculate the determinant of a matrix.

    Parameters:
    Input matrix m of shape (n, n).

    Returns:
    Determinant of the input matrix.
    """
    
def rank(m):
    """
    Function to calculate the rank of a matrix.

    Parameters:
    Input matrix of shape (n, n).

    Returns:
    Rank of the input matrix.
    """# -*- coding: utf-8 -*-

