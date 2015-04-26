## Programming Assignment 2

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL 
    set <- function(y) {
           x <<- y
           m <<- NULL
    }
    get <- function() x
    setinverse <- function(x) m <<- x
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse (or get the cached result when available)
## of the "matrix" returned by makeCacheMatrix above


cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m        
}
