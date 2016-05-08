# Allows creation of object wrapper for matrices using makeCacheMatrix which caches computation of inverse when using cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  # Adds caching fields to lexical scope for inverse calculation
  #
  # Args:
  #   x: invertible square matrix
  #
  # Returns:
  #   Object wrapping matrix which caches inverse computation
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Computes inverse matrix if not cached
  #
  # Args:
  #     x: wrapped invertible square matrix obtained through makeCacheMatrix
  #   ...: additional arguments for solve(...) function
  #
  # Returns:
  #   Inverse of matrix
  i <- x$getinverse()
  if (!is.null(i))
    return(i)
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
