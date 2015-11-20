## Functions to compute the inverse of a matrix
## with caching of the result.
## The matrix used must be invertible

## Usage:
## Set the matrix with
##  cacheMatrix <- makeCacheMatrix(myMatrix)
##
## Passing myMatrix is optional - 
## The matrix to use can be set or changed with
##  cacheMatrix$set(myMatrix)
##
## Then use cachSolve to return the cached result if available
## or compute and return the inverse, caching the result for
## subsequent calls to cacheSolve
##  cacheSolve(cacheMatrix)

## Returns a list of functions to be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the cached inverse of a matrix if available
## or computes the inverse and returns the result which
## is cached for subsequent calls

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
