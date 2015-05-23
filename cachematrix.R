## makeCacheMatrix(x)
## This function creates a special "matrix" object that can cache its inverse.
## The x variable is the original matrix. The s variable is its inverse(if 
## setSolve() ever called by function cacheSolve(x)). This function returns
## a list of functions to get/set the original matrix and the inversed one.
## 
## cacheSolve(x)
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the 
## cache.
## The x variable is the special "matrix" object created by function 
## makeCacheMatrix. The function returns the inverse of the original matrix.
##

## creat a cached matrix for the matrix x

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## solve the cached matrix x or just return the cached one

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
