## Function of makeCacheMatrix and cacheSolve
## Function making concepts are taken from different Github pages

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # Matrix value
  set <- function(y) {
    x <- y
    inverse <- NULL
  }
  # Required matrix value
  get <- function() x
  # Inverse function
  set_inverse <- function(inverse1) inverse <<- inverse1
  # Inverse value
  get_inverse <- function() inverse
  
  # Listing
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("Cached Data Shown")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}

