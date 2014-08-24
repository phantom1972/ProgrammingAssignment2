##  this file contains a pair of functions that cache the inverse of a matrix

##   makeCacheMatrix
##
##   Descripion:
##       this function creates a special matrix object 
##       that can cache its inverse
##   Usage:
##       makeCacheMatrix(x)
##   Arguments
##       x - an R matrix
##   Example
##       m <- makeCacheMatrix(matrix (1:4, nrow = 2, ncol = 2))

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    setinverted <- function(invertedMatrix) inverted <<- invertedMatrix
    getinverted <- function() inverted
    list(set = set, get = get,
         setinverted = setinverted,
         getinverted = getinverted)
}


##   cacheSolve
##
##   Descripion:
##       this function computes the inverse of a matrix returned by makeCacheMatrix
##       if the inverse has already been calculated and the matrix has not changed, 
##       then the cacheSolve retrieves the inverse from the cache
##   Usage:
##       cacheSolve(x, ...)
##   Arguments
##       x - a matrix that created by makeCacheMatrix
##       ... - further arguments passed to or from other methods
##   Example
##       m <- makeCacheMatrix(matrix (1:4, nrow = 2, ncol = 2))
##       m_inverted <- cacheSolve(m)

cacheSolve <- function(x, ...) {
    m <- x$getinverted()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverted(m)
    m
}
