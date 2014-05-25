## cachematrix.R
## Use lexical scoping to memoize a potentially
## long running computation
## in this case, matrix inversion

## makeCacheMatrix creates a list
## containing the functions to get/set a matrix
## uses the <<- operator to cache values to be
## looked up later
## The inverted matrix will come through the getmatrix function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve
## check the global x value
## if it exists, return that
## otherwise, calculate the inverted matrix
## and store it.
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}