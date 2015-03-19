## This file was created in order to solve the Assignment number 2 of the
## rprog-012 Coursera course : "R Programming", 2015.
## The following 2 functions create a special object that stores a matrix
## and chaches its inverse.

## The function makeCacheMatrix receives a matrix as input and
## produces a list of functions as an output. Details on the
## behaviours of the functions follow as comments.

makeCacheMatrix <- function(x = matrix()) {
   invmatrix <- NULL
   # set() re-assigns a new matrix to x and re-initialize invmatrix
   set <- function(y){
     x <<- y
     invmatrix <- NULL
   }
   # get() outputs the initial matrix
   get <- function() x
   # setInv() assigns the inverse matrix to invmatrix
   setInv <- function(inverse) invmatrix <<- inverse
   # getInv() outputs the inverse matrix
   getInv <- function() invmatrix
   list(set = set, get = get,
       setInv = setInv, getInv = getInv)
   }


## The following functions receives the output 
## of makeCacheMatrix and checks if the inverse matrix is already
## stored in invmatrix. If it is, it returns it. Otherwise, it calculates
## the inverse, stores it in invmatrix and outputs the inverse.

cacheSolve <- function(x, ...) {
   invmatrix <- x$getInv()
   if(!is.null(invmatrix)) {
      message("getting cached data (inverse matrix)")
      return(invmatrix)
   }
   ## Assignment of the matrix to data via the get() function
   ## defined in makeCacheMatrix
   data <- x$get()
   ## Inverse calculation
   invmatrix <- solve(data)
   ## Assignment of the inverse to invmatrix
   x$setInv(invmatrix)
   invmatrix
}
