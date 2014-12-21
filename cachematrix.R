## The following functions provide time-saving computations
## for matrix objects

## makeCacheMatrix "saves" the matrix elements and the inverse matrix,
## made from it, in a cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  data.frame(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function checks if the inverse matrix was created,
## if so, returns it, if not, makes the inversion and prints
## the inverse matrix
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m        ## Return a matrix that is the inverse of 'x'
}
