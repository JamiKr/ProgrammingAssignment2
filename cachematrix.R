## These are functions which handle the inversion and caching of
## inversion of matrices

## Set of functions to manage the matrix inversion cache.
## Use this function to set up the matrix with the methods.  Make
## sure you assign the result to a new R object, then pass that
## resulting R object into cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
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
