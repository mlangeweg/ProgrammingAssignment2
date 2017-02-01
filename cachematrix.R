## The following functions will cache the inverse of a matrix.
## These functions meet the assumption that the supplied matrix is always
## invertible.

## The "makeCacheMatrix" function creates a matrix object that
## is able cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <-  function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse =
                     getinverse)
}

## The "cacheSolve" function returns the inverse of the matrix created by the
## "makeCacheMatrix" function.  If the inverse of the matrix has already been
## calculated, it passes that result from cache and skips further computation.
## If the computation has not been passed to cache, this function will calculate
## the inverse of the matrix, and then set it to cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data.")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
