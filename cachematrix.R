## These are functions which handle the inversion and caching of
## inversion of matrices

## Set of functions to manage the matrix inversion cache.
## Use this function to set up the matrix with the methods.  Make
## sure you assign the result to a new R object, then pass that
## resulting R object into cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(result) inverse <<- result
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Matrix inversion function wrapper which first checks for
## answer in cache before continuing

cacheSolve <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
