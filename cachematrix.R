## R-Programming: Programming Assignment 2


## Function 'makeCacheMatrix' creates a special "matrix" object 
## that can cache its inverse.
## Computing the inverse of a matrix is done by the function 'solve' in R

## Input example:
## > tmp<-makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Function 'cacheSolve' computes the inverse of the special "matrix" 
## returned by the function 'makeCacheMatrix'. If the inverse has already 
## been calculated (and the matrix has not changed), then 'cacheSolve'
## should retrieve the inverse from the cache.

## Input example:
## > cacheSolve(tmp)

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}