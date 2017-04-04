## makeCacheMatrix creates an object of type makeCacheMatrix() that
## stores a matrix (provided as a formal parameter by user) & provides
## 4 methods for operating on the object. [1] set() allows the user to
## reset the matrix. [2] get() allows the user to get the value of the matrix.
## [3] setinv() allows the function cacheSolve() to cache the inverse of the
## matrix. [4] getinv allows the user to get the value of the local var 'inv',
## which is the inverse that's cached by cacheSolve() when cacheSolve() is
## given an argument of type makeCacheMatrix().

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv.cacheSolve) inv <<- inv.cacheSolve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve() caches the inverse of the matrix stored in the object of
## type makeCacheMatrix(). It's assumed that the matrix provided as a formal
## param to makeCacheMatrix is always invertible. See previous comment for
## further details. Example use of these 2 functions:
##
##      matrix1 <- matrix(c(2,2,3,2), nrow = 2)
##      myMatrix <- makeCacheMatrix(matrix1)
##      cacheSolve(myMatrix)

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv.cacheSolve <- solve(data, ...) # inverse of the matrix.
        x$setinv(inv.cacheSolve)
        inv.cacheSolve
}
