#############################################################
## Solution to Assignment 2 by Alvaro Calle Cordon
## Date: 25th January 2015
##
## These two functions cache the inverse of a given matrix
#############################################################

makeCacheMatrix <- function(x = matrix()) {
# ---------------------------------------------------
# Creates a matrix object that can cache its inverse
# ---------------------------------------------------
      inv <- NULL
      
      # set the value of the matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      
      # get the value of the matrix
      get <- function() x
      
      # set the value of the inverse
      setinv <- function(solve) inv <<- solve
      
      # get the value of the inverse
      getinv <- function() inv
      
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
# ---------------------------------------------------
# Return a matrix that is the inverse of 'x'
# Remarks:
#     * uses solve(x)=x^(-1) to compute the inverse
#     * x is assumed to be always invertible
# ---------------------------------------------------
      inv <- x$getinv()
      
      # if inverse has been calculated get it:
      if(!is.null(inv)) { 
            message("getting cached data")
            return(inv)
      }

      # if not, calculate inverse:      
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
