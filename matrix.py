
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
    res = np.copy(m).astype(float)
    flag=1
    scalar=1.0
    inverted= np.identity(n)
    while row < n and col < n:
        pivot_row = find_pivot_row(res, row, col)
        if pivot_row!=None:
            # swap the rows
            inverted[row], inverted[pivot_row] = inverted[pivot_row], inverted[row]
            res[row], res[pivot_row] = res[pivot_row], res[row]

            # divide the rows elements with the first nonzero element 
            scalar /=res[row][col]
            inverted[row] /= res[row][col]
            res[row] /= res[row][col]
            if pivot_row != row: 
                flag *=-1
            
            # Reset all other elements in the current column
            for r in range(n):
                if r!= pivot_row:
                    factor = res[r, col]
                    inverted[r] -= factor * inverted[pivot_row]
                    res[r] -= factor * res[pivot_row]
                    
            row+=1
        col+=1
      
       
    return (res,scalar,flag,inverted)    
    
def find_pivot_row(matrix, start_row, col):
    """
    Function to find the pivot row in a given column of the matrix.

    Parameters:
    matrix (numpy.ndarray): Input matrix of shape (m, n).
    start_row (int): Starting row index for search.
    col (int): Column index for pivot search.

    Returns:
    int: Index of the pivot row, or None if no pivot is found.
    """
    num_rows = matrix.shape[0]
    for row in range(start_row, num_rows):
        if matrix[row, col] != 0:
            return row
    return None 
    
    



    
def determinant(m,scalar,flag):
    
    """
    Function to calculate the determinant of a matrix.

    Parameters:
    Input matrix m of shape (n, n).

    Returns:
    Determinant of the input matrix.
    """

    n=m.shape[0]
    det=1
    for i in range(n):
        det *= m[i][i]
        if det==0:
            return det
    if det!=0:
        det*=flag
        det/=scalar
        #add the multiplcity of the scalar from the gaus elimination if needed 
        pass 
    return det 
        


def rank(m):
    """
    Function to calculate the rank of a matrix.

    Parameters:
    Input matrix of shape (n, n).

    Returns:
    Rank of the input matrix.
    """# -*- coding: utf-8 -*-

   
    rank_m=0
    n=m.shape[0]
    for row in range(n):
        #scanning the diagnale and up only 
        for col in range(row, n):
            #change to canonial
            #if canonial_ranked[row][col]!=0:
            if m[row][col]!=0:
                rank_m +=1
                break
                
    return rank_m



def matrix_method(m):
    
    echelon_form,scalar,flag,inver=gaussian_elimination(m)
    det=determinant(echelon_form, scalar,flag)
    rank_m=rank(echelon_form)
    print("\n\MY METHODS: ")
    print("Rank of the matrix:", rank_m)
    print("Determinant of the matrix:", det)
    if det!=0:
        print("Invert of the matrix:\n", inver)

# Example usage:
#matrix = np.random.randint(1000,size=(1000,1000))
def pymethods(matrix):
    det=np.linalg.det(matrix)
    r=np.linalg.matrix_rank(matrix)
    if det!=0:
        inv=np.linalg.inv(matrix)
    print("\n\nPYTHON METHODS: ")
    print("Rank of the matrix:",r)
    print("Determinant of the matrix:", det)
    if det!=0:
        print("Invert of the matrix:\n", inv)
print("1.det_matrix(800 x 800).txt")       
matrix= np.loadtxt('det_matrix(800 x 800).txt', usecols=range(800))
t=time()
matrix_method(matrix)
print("TIME:",time()-t)


t=time()
pymethods(matrix)
print("TIME:",time()-t)


print("2.inv_eig_matrix(800 x 800).txt")      
matrix= np.loadtxt('inv_eig_matrix(800 x 800).txt', usecols=range(800))
t=time()
matrix_method(matrix)
print("TIME:",time()-t)


t=time()
pymethods(matrix)
print("TIME:",time()-t)

print("3.rank_matrix(1000x1000).txt")
matrix = np.loadtxt('rank_matrix(1000x1000).txt', delimiter=',', usecols=range(1000))


t=time()
matrix_method(matrix)
print("TIME:",time()-t)


t=time()
pymethods(matrix)
print("TIME:",time()-t)

