## This pair of functions is used to cache the inverse of a square matrix.
## They allow to save time by looking up cache rather than recomputing.

## The function "makeCacheMatrix" creates a cacheable matrix used as an input
## into the "cacheSolve" function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## "cacheSolve" function uses the output of the previous "makeCacheMatrix"
## function to compute the inverse of the original matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
