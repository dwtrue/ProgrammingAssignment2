## This function creates a special matrix object that can cache its inverse
## tested using: x<-matrix(rnorm(1:49), nrow=7)

## See comments below for details on how to use this with cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <- function(y) {
                x<<- y
                m<<-NULL
        }
        get <-function() x
        setSolve<-function(solve) m<<-mean
        getSolve<-function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function computes the inverse of the matrix returned by the function
## makeCacheMatrix (see above). If the inverse has already been calculated
## (and the matrix has not changed) the cachesolve retrieves the inverse
## from the cache.

## USE: cacheSolve(makeCacheMatrix(x))

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
