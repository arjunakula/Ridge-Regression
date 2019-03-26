# -*- coding: utf-8 -*-
"""
Python code for ridge and spline regression

"""

import numpy as np

def mySweep(A, m):
    """
    Perform a SWEEP operation on A with the pivot element A[m,m].
    
    :param A: a square matrix.
    :param m: the pivot element is A[m, m].
    :returns a swept matrix. Original matrix is unchanged.
    """
    B = np.copy(A)   
    n = B.shape[0]
    for k in range(m):
        for i in range(n):
            for j in range(n):
                if i!=k and j!=k:
                    B[i,j] = B[i,j] - B[i,k]*B[k,j] / B[k,k]
        for i in range(n):
            if i!=k:
                 B[i,k] = B[i,k] / B[k,k]
        for j in range(n):
            if j!=k:
                B[k,j] = B[k,j] / B[k,k]
        B[k,k] = -1/B[k,k]
    
    return(B)
    
    
def ridge(X, y, lam):
    n, p = X.shape
    Z = np.column_stack((np.ones((n,1)), X, y))
    A = Z.T.dot(Z)
    D = np.eye(p+2) * lam
    D[0, 0] = 0
    D[-1, -1] = 0
    A = A + D
    S = mySweep(A, p+1)
    beta = S[0:p+1, p+1]
    return beta   
    
    
n = 100
sigma = 0.1

# Generate a set of true two dimensional points (x, y)
# lies around a quadratic spline.
x = np.sort(np.random.uniform(size=n))
y = x**2 + np.random.standard_normal(size=n)*sigma

## Might take a minute or two to run
def spline(x, beta=None, p=500, lam=10):
    # Generate p-1 additional features.
    X = np.copy(x)
    n = x.shape[0]
    for k in (np.arange(1, p, dtype='float')/p):
        X = np.column_stack((X, (x > k)*(x - k)))

    if beta is None:
        # Fit a spline using ridge regression.
        beta = ridge(X, y, lam)
    y_hat = np.hstack((np.ones((n, 1)), X)).dot(beta)
    return y_hat, beta

y_hat, beta = spline(x)   


# Plot the original data and regression results.
import matplotlib.pyplot as plt
#get_ipython().magic(u'matplotlib inline')

plt.figure()
plt.plot(x, y, color='blue', label='Data')
plt.plot(x, y_hat, color='red', label='Spline Regression', linewidth=2)
plt.legend(loc=2)
plt.xlim(0, 1)
plt.ylim(-0.2, 1.2)
plt.show()
