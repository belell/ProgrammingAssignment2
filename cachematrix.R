## The two functions enable the caching of the potentially time-consuming computation of the inverse of a matrix.
## This is done by calculating the inverse once and caching the value. The next time the value is needed the cache is
## accessed rather than performing a recalculation.
## A strategy similar to object-oriented programming is used.


## The makeCacheMatrix function uses as input a matrix of which the inverse needs to be calcualted.
## It creates a list of functions for setting and getting the cached value.
## The value of the inverse matrix is stored in the calling environment by the superassignment operator ("<<-")


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


## The cacheSolve function uses as input the list created by the makeCacheMatrix. It first checks if
## the value had been calculated previously. If not, it calculates the inverse and stores it using the
## a function of the makeCacheMatrix. If the value had been calculated previously it returns the value
## from the "cache"

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
