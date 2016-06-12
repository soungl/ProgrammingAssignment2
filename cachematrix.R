## The following two functions below are written to cache a calculated 
## value so that it doesn't have to compute again.

## makeCacheMatrix creates a special vector that caches a computed inverse
## of a matrix x. It doesn't compute the inverse itself.

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


## cacheSolve returns the cached inverse of x. If its inverse wasn't already computed
## calcuated, it computes and caches the inverse using x$setinverse(m).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m}
