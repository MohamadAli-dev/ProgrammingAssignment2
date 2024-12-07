## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # function to set the matrix value
    x <<- y
    inv <<- NULL # Clear the cached inverse when the matrix is reset
  }
  get <- function()
    x # Function to get the matrix
  setInverse <- function(inverse)
    inv <<- inverse # cache the inverse
  getInverse <- function()
    inv # Retrieve the cached inverse
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  ) # Return the list of functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() # Check for a cached inverse
  if(!is.null(inv)) {
    message("Getting cached inverse") # Notify user of cache usage
    return(inv) # Return the cached inverse
  }
  mat <- x$get() # Retrieve the matrix
  inv <- solve(mat, ...) # Compute the inverse
  x$setInverse(inv) # Cache the computed inverse
  inv # return the inverse
}
