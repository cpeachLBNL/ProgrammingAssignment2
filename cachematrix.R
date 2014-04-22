## Purpose:  These functions provide the capability to cache a matrix and 
##   its inverse, which can improve performance for larger matrixes that 
##   are frequently inverted.
##   Steps to use:
##   1) Call makeCacheMatrix to create a cached matrix
##   2) Pass the returned object into cacheSolve to get the inverse.


## The makeCacheMatrix function receives a matrix and returns a list of
##   4 functions:  
#      - set and get the matrix, and
##     - set and get the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverseMatrix to NULL
        inverseMatrix <- NULL
        set <- function(y) {
                ## Assign the new value to parent's x variable
                x <<- y 
                ## Assign NULL to parent's inverseMatrix variable
                inverseMatrix <<- NULL  
        }
        get <- function() x
        setInverse <- function(inverse) inverseMatrix <<- inverse
        getInverse <- function() inverseMatrix
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## The cacheSolve function receives a list object generated from the
##    makeCacheMatrix function and returns the inverse matrix by:
##    1) Returning the cached inverse matrix if it exists, OR
##    2) Calling solve() on the cached matrix to calculate the inverse

cacheSolve <- function(x, ...) {
        ## Get the cached inverse matrix
        inverseMatrix <- x$getInverse()
        ## If the cached inverse matrix exists, then return it
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        ## Cached inverse matrix does not exist - calculate and return it
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}
