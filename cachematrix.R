## Put comments here that give an overall description of what your
## functions do

## This function receives a matrix, stores it in a cache
## and also stores the inverse in order to improve the
## performance of a program that needs to calculate multiple
## times that inverse.
makeCacheMatrix <- function(x = matrix()) {
  theInverse <- NULL
  
  set <- function(theMatrix) {
    x <<- theMatrix
    theInverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) {
    theInverse <<- inverse
  }
  
  getInverse <- function() theInverse
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## This function receives a cached matrix created with the
## makeCacheMatrix function and returns the inverse.
## If the inverse is in the cache, doesn't recalculate it.
## Else, calculates the inverse and store it in the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  theInverse <- x$getInverse()
  if(!is.null(theInverse)) {
    message("getting cached data")
    return(theInverse)
  }
  
  theMatrix <- x$get()
  theInverse <- solve(a=theMatrix)
  x$setInverse(theInverse, ...)
  theInverse
}
