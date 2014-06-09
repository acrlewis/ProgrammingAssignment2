## Coursera - R Programming - Johns Hopkins
## Programming Assignment 2
## Function of makeCacheMatrix and cacheSolve
## Function making concepts are taken from different Github pages

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # Matrix value
  set <- function(y) {
    x <- y
    inverse <- NULL
  }
  # Required matrix value
  get <- function() x
  # Inverse function
  set_inv <- function(inv1) inv <<- inv1
  # Inverse value
  get_inv <- function() inv
  
  # Listing
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inverse)) {
    message("Cached Data Shown")
    return(inverse)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inverse)
  inverse
}

