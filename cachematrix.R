## This pair of functions calculates and caches the inverse of an arbitrary square matrix.

## How to use it?
## 1. Call 'makeCacheMatrix' function with a square matrix as attribute, and 
##    assign its output to a variable.
## 2. Call 'cacheSolve' function with the previously defined variable as attribute.
## 
## If you want to provide the inverse of a matrix multiple times, just repeat Step 2, and 
## the inverse is given from the cache.

## Drawbacks:
## - if you call 'cacheSolve(makeCacheMatrix(x))', it always clears the cache and
##    calculates the inverse
## - if you call 'makeCacheMatrix(x)' multiple times with the same attrinute, 
##    it always clears the cache  

## 'makeCacheMatrix' gives back a list of methods to cache and get the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
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