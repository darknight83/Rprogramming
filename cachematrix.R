## Programming Assignment 2 - R Programming
## ---------------------------------------------------------------------------------------------------
## Overview: Matrix inversion is usually a costly computation
##          and there may be some benefit to caching the inverse 
##          of a matrix rather than computing it repeatedly. 
## ---------------------------------------------------------------------------------------------------
## Functions: 
##     1. makeCacheMatrix:This function creates a special "matrix" object that can cache its inverse.

## ---------------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL                                                    ## Initialize mat_inv to Null
  set <- function(y) {                                               ## Create set function
    x <<- y 
    mat_inv <<- NULL
  }
  
  get <- function() x                                                ## Create get function
  setinverse <- function(inverse) mat_inv <<- inverse
  getinverse <- function() mat_inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ## Return all functions
}


## ---------------------------------------------------------------------------------------------------
## Functions: 
##     2. cacheSolve: This function computes the inverse of the special "matrix" returned by
##                    makeCacheMatrix above. If the inverse has already been calculated  
##                    (and the matrix has not changed), then cacheSolve should retrieve 
##                    the inverse from the cache.
## ---------------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    mat_inv <- x$getinverse()        
    if(!is.null(mat_inv)) {          ## Check for calculated inverse 
      message("getting cached data.")
      return(mat_inv)
    }
    calc <- x$get()                ## Calc inverse when not available
    mat_inv <- solve(calc)
    x$setinverse(mat_inv)
    mat_inv
}

  