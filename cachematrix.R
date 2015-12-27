## Description: Functions to make and return a cached result for inversing matrices.
## Author: Andrew Borst
## Date: 12/25/2015
 
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the values of the matrix
# get the values of the matrix
# set the inverse of the matrix
# get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(invM) m <<- invM
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  m
}

