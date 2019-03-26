############################################################# 
## Stat 202A - Homework 2
## Author: 
## Date : 
## Description: This script implements ridge regression as 
## well as piecewise linear spline regression.
#############################################################

#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names, 
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to 
## double-check your work, but MAKE SURE TO COMMENT OUT ALL 
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not use the function "setwd" anywhere
## in your code. If you do, I will be unable to grade your 
## work since R will attempt to change my working directory
## to one that does not exist.
#############################################################

#################################
## Function 1: Sweep operation ##
#################################

mySweep <- function(A, m){
  
  # Perform a SWEEP operation on A with the pivot element A[m,m].
  # 
  # A: a square matrix.
  # m: the pivot element is A[m, m].
  # Returns a swept matrix.
  
  ## Leave this function as is unless you want to make it 
  ## more efficient!
  
  n <- nrow(A)
  
  for(k in 1:m){ 
    for(i in 1:n)     
      for(j in 1:n)   
        if(i != k  & j != k)     
          A[i,j] <- A[i,j] - A[i,k]*A[k,j]/A[k,k]    
        
        for(i in 1:n) 
          if(i != k) 
            A[i,k] <- A[i,k]/A[k,k]  
          
          for(j in 1:n) 
            if(j != k) 
              A[k,j] <- A[k,j]/A[k,k]
            
            A[k,k] <- - 1/A[k,k]
  }
  
  return(A)
  
}


##################################
## Function 2: Ridge regression ##
##################################

myRidge <- function(X, Y, lambda){
  
  # Perform ridge regression of Y on X.
  # 
  # X: a matrix of explanatory variables.
  # Y: a vector of dependent variables. Y can also be a 
  # matrix, as long as the function works.
  # lambda: regularization parameter (lambda >= 0)
  # Returns beta, the ridge regression solution.

  ##################################
  ## FILL IN THIS SECTION OF CODE ##
  ##################################
  n = dim(X)[1]
  p = dim(X)[2]
  
  Z = cbind(rep(1,n),X,Y)
  A = t(Z) %*% Z
  
  D = diag(rep(lambda,p+2))
  D[p+2, p+2] = 0
  D[1][1] = 0
  A = A+D
  
  S = mySweep(A,p+1)
  beta_ridge = S[1:(p+1), p+2]
  
  ## Function should output the vector beta_ridge, the 
  ## solution to the ridge regression problem. Beta_ridge
  ## should have p + 1 elements.
  return(beta_ridge)
}


##############################################################
## Optional function (not graded, just for testing myRidge) ##
##############################################################

testRidge <- function(){
  
  ## You can edit this function however you like (or not at all). 
  ## I just included it here to help you check if your ridge 
  ## function is working properly.
  
  ## Load up "trees" data
  my.data <- trees
  
  ## Let X be the Girth and Height of trees
  ## X needs to be a matrix
  X <- as.matrix(trees[,1:2])
  
  ## Let Y be the Volume of the tree
  Y <- trees$Volume
  
  ## Compare ridge solution when lambda = 0 to ordinary linear regression
  beta_ls    <- lm(Volume ~ Girth + Height, data = my.data)$coeff
  beta_ridge <- myRidge(X, Y, 0)
  
  #output_spline <- mySpline(X,Y,0)
  
  if(sum(beta_ridge - beta_ls) <= 10^(-5)){
    cat("Looks like myRidge is working properly when lambda = 0! \n")
  }
  ## You can test around some more
}


####################################################
## Function 3: Piecewise linear spline regression ##
####################################################


mySpline <- function(x, Y, lambda, p = 100){
  
  # Perform spline regression of Y on x.
  # 
  # x: An n x 1 vector or n x 1 matrix of explanatory variables.
  # You can assume that 0 <= x_i <= 1 for i=1,...,n.
  # Y: An n x 1 vector of dependent variables. Y can also be an 
  # n x 1 matrix, as long as the function works.
  # lambda: regularization parameter (lambda >= 0)
  # p: Number of cuts to make to the x-axis.

  ##################################
  ## FILL IN THIS SECTION OF CODE ##
  ##################################
  
  n = nrow(as.matrix(x))
  X = matrix(x, nrow=n)
  
  for (k in ((1:(p-1))/p)){
       X = cbind(X,(x>k)*(x-k))
  }
  
  beta_spline = myRidge(X,Y,lambda)
  
  y.hat = cbind(rep(1,n),X) %*% beta_spline
  
  ## Function should return a list containing two elements:
  ## The first element of the list is the spline regression
  ## beta vector, which should be p + 1 dimensional (here, 
  ## p is the number of cuts we made to the x-axis).
  ## The second element is y.hat, the predicted Y values
  ## using the spline regression beta vector. This 
  ## can be a numeric vector or matrix.
  output <- list(beta_spline = beta_spline, predicted_y = y.hat)
  return(output)
  
}


########################################################
## Optional examples (comment out before submitting!) ##
########################################################
#testRidge()

# n = 1000
# sigma = 0.15
# x = runif(n)
# x = sort(x)
# Y = x^2 + rnorm(n)*sigma
# lambda = 0 # here I set different lambda values for plotting
# plot(x,Y,ylim=c(-0.2,1.2),col="red")
# par(new=TRUE)
# output_spline <- mySpline(x,Y,lambda)
# Yhat = as.vector(output_spline$predicted_y) 
# plot(x,Yhat,ylim=c(-0.2,1.2),col="blue")
# 
# # compute training error
# error = 0
# for (match in 1:1000){
#   training_error = training_error + abs(Yhat[match]-Y[match])
# }

# lambda_list = c(0, 0.01, 0.05, 0.1, 0.2, 0.5, 1, 10, 20, 30, 40, 80, 100, 300, 500, 1000, 2000)
# #lambda_list = c(0, 0.01, 0.05)
# 
# tr_error = c()
# tt_error = c()
# 
# for (q in 1:length(lambda_list))
# {
# n = 2000
# sigma = 0.1
# x = runif(n)
# #x_random = sample(x)
# 
# n_train = 1000
# n_test = 1000
# 
# x_train = x[1:1000]
# x_test = x[1001:2000]
# 
# x = sort(x_train)
# Y = (x_train)^2 + rnorm(n_train)*sigma
# 
# x_test = sort(x_test)
# Y_test = (x_test)^2 + rnorm(n_test)*sigma
# 
# lambda = lambda_list[q]
# 
# output_spline <- mySpline(x,Y,lambda)
# 
# # testing data
# n_test = nrow(as.matrix(x_test))
# XTest = matrix(x_test, nrow=n_test)
# p = 100
# for (k in ((1:(p-1))/p)){
#   XTest = cbind(XTest,((x_test)>k)*((x_test)-k))
# }
# yTrainHat = as.vector(output_spline$predicted_y)
# yTestHat = as.vector(cbind(rep(1,n_test),XTest) %*% output_spline$beta_spline)
# 
# # compute training error
# training_error = 0
# for (match in 1:1000){
#     training_error = training_error+((yTrainHat[match]-Y[match])^2);
# }
# 
# # compute testing error
# testing_error = 0
# for (match in 1:1000){
#     testing_error = testing_error+((yTestHat[match]-Y_test[match])^2)
# }
# 
# tr_error[q] = training_error
# tt_error[q] = testing_error
# 
# }
# 
# plot(lambda_list, tr_error,col="red")
# lines(lambda_list, tr_error)
# par(new=TRUE)
# plot(lambda_list, tt_error,col="blue")
# lines(lambda_list, tt_error)

# 
# # plot(x,Y,ylim=c(-0.2,1.2),col="red")
# # par(new=TRUE)
# # output_spline <- mySpline(x,Y,lambda)
# # Yhat = as.vector(output_spline$predicted_y)
# # plot(x,Yhat,ylim=c(-0.2,1.2),col="blue")