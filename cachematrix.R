## This pair of functions is used to cache the inverse of a square matrix.
## They allow to save time by looking up cache rather than recomputing.

## The function "makeCacheMatrix" creates a cacheable matrix used as an input
## into the "cacheSolve" function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## Set value of matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get value of matrix
    get <- function() x
    
    ## Set value of inverse matrix
    setinv <- function(solve) m <<- solve
    
    ## Get value of inverse matrix
    getinv <- function() m
    
    ## Create input for cacheSolve (cacheable matrix)
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## "cacheSolve" function uses the output of the previous "makeCacheMatrix"
## function to compute the inverse of the original matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    ## Check if the inverse matrix is available, if so skips calculation
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Create an inverse matrix if not available
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
