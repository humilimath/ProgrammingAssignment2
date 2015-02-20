## These functions save computational work in calculating the inverse
## of a matrix by caching the calculation of the matrix. 
##
## The makeCacheMatrix sets this up by wrapping a matrix in an object which 
## can later be acted on by the cacheSolve command to either calculate and return
## the inverse or report the cached value.
##
## These functions also show off lexical scoping and the <<- operation.

## Function creates and object wrapper for a matrix.  The Object (which is actually
## a list) has methods (list entries which are functions) that get and set the 
## underlying matrix and get and set the associated inverse.

makeCacheMatrix <- function(x = matrix()) {
     mat <- x  # set the underlying matrix
     inverse <- NULL # default for the underlying inverse
     
     ## get function to return the underlying matrix
     get <- function() mat
     
     ## set function to set/change the underlying matrix
     set <- function(y = matrix()) {
          mat <<- y       ## set the underlying value using <<- to get the value in the makeCacheMatrix
          inverse <<- NULL  ## reset the inverse
     }
     
     ## get function for the inverse
     getInverse <- function()inverse
     
     # set function for the inverse
     setInverse <- function(inv = matrix()){
          inverse <<- inv  ##set the underlying value using <<-
     }
     ##return the functions as a list
     list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
     
}


## solves for the inverse of the underlyingn matrix for an object(list) produced
## by mackeCache
## Assumes the matrix that underlies x is invertable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
        ## first check to see if the inverse has already been calculated.  If so,
        ## return the cached value
        if(!is.null(x$getInverse())){
             message("Getting cached Data")
             return(x$getInverse())
        }
        
        ## Otherwise calculate it and store it away in the matrix wrapper object
        inv <- solve(x$get())
        x$setInverse(inv)
        inv ## report the inverse as the return value for the function
}
