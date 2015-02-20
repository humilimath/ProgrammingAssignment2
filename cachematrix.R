## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     mat <- x
     inverse <- NULL
     get <- function() x
     set <- function(y = matrix()) {
          x <<- y
          inverse <<- NULL
     }
     getInverse <- function()inverse
     setInverse <- function(inv = matrix()){
          inverse <<- inv
     }
     list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
     
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(x$getInverse())){
             message("Getting cached Data")
             return(x$getInverse())
        }
        inv <- solve(x$get())
        x$setInverse(inv)
        inv
}
