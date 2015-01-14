## Programming assignment 2
##   nvparrish
##
## The following two functions demonstrate how to perform caching using
## scope within R code.  The following test code demonstrates working behavior
##
##  x = matrix(c(2,3,2,2), nrow=2, ncol=2, byrow=TRUE)
##  y = makeCacheMatrix(x)
##  cacheSolve(y)
##  cacheSolve(y)
##
##  This code creates a simple matrix, creates the cached matrix, and then
##  calls the cacheSolve function twice.  The first time calculates the
##  inverse directly, the second uses the cached version.  

## makeCacheMatrix() - This function creates a special matrix object that
##   can cache its inverse.
## INPUTS:
##   x: An invertible matrix (i.e. non-singular, square matrix)
## OUTPUT:
##   Returns a list of pointers to utility functions for getting and setting
##   values for the matrices.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    #Function to set the value of the matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    #Function to get the cached matrix
    get <- function() {
        x
    }

    ##Function to set the inverse
    setInverse <- function(mInverse) {
        inverse <<- mInverse
    }

    ##Function to get the inverse
    getInverse <- function() {
        inverse
    }

    ##Return a list of the helper functions
    list(set = set, 
        get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve() - This function computes the inverse of the special "matrix"
##   returned by makeCacheMatrix().  If the inverse has already been calculated
##   (and the matrix has not changed), then the cachesolve should retrieve the
##   inverse from the cache.
## INPUTS:
##   x: An invertible matrix (i.e. non-singular, square matrix)
##   ... : Additional parameters to be passed to the solve() function 
## OUTPUT:
##   Returns the (possibly cached) version of the result of solve(x,...)
cacheSolve <- function(x, ...) {
    ## Get the currently cached version of x
    inverse <- x$getInverse()

    ## If the cached version is available, use that
    if(!is.null(inverse)){
        message("Getting cached data")
        return(inverse)
    }

    ## No cached version was available, so calculate and cache it
    message("Calculating new value")
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
