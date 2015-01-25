## This pair of functions calculates and caches the inverse of an arbitrary square matrix.

## How to use it?
## 1. Call 'makeCacheMatrix' function with a square matrix as attribute, and 
##    assign its output to a variable, e.g. 'var'.
## 2. Call 'cacheSolve' function with the previously defined variable ('var') as attribute.
## 
## Change the matrix with calling var$set(<new matrix>), then repeat Step 2.

## 'makeCacheMatrix' gives back a list of methods to cache and get the inverse matrix
## if a new matrix is assigned via the 'set' function, it is compared to the previous one, and
## the cache is cleared only if the two matrices are not identical

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            if (!identical(x,y)) {
                  x <<- y
                  m <<- NULL
            } 
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## 'cacheSolve' gives back the inverse matrix either from cash or via calculating it

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