# This function, makeCachMatrix is a list containing a function to:
# set the matrix
# get the matrix
# set the inverse of the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

# This function, cacheSolve, returns the inverse of a matrix
# unless it has already been calculated, in which case
# it simply returns the cached value to save time (see test below)

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}