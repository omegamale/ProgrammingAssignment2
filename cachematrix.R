## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.
## I wrote the following functions to do so. makeCacheMatrix creates a special
## "matrix" object that can cache its inverse. cacheSolve computes the inverse
## of the special "matrix" returned by makeCacheMatrix. If the inverse has
## already been calculated (and the matrix has not changed), then the cacheSolve
## will retrieve the inverse from the cache. Please note, that the makeCacheMatrix
## has to be called first (before calling cacheSolve).

## This function creates a special "matrix"
## object that can cache its inverse. It is used to:
## 1. Set the value of the initial matrix;
## 2. Get the value of the initial matrix;
## 3. Set the value of the inverted matrix;
## 4. Get the value of the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    	x <<- y
    	m <<- NULL
    }
    get <- function() x
    setInv <- function(Inv) m <<- Inv
    getInv <- function() m
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x) {
    m <- x$getInv()
    if(!is.null(m)){
        message("Getting cached data.")
        return(m)
    }
    else {
        message("No cached data found. Inverting matrix...")
        data <- x$get()
        m <- solve(data)
        x$setInv(m)
        message("Done.")
        return(m)
    }
}

## Test:
## > x = cbind(c(1:2), c(3:4))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(m)
## No cached data found. Inverting matrix...
## Done.
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## Getting cached data.
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5