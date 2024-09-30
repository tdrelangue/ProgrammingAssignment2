## Put comments here that give an overall description of what your
## functions do

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # Initialize the inverse as NULL
  set <- function(y) {
    x <<- y  # Set the matrix value
    m <<- NULL  # Reset the cached inverse
  }
  get <- function() x  # Get the matrix value
  setInverse <- function(inverse) m <<- inverse  # Set the cached inverse
  getInverse <- function() m  # Get the cached inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  m <- x$getInverse()  # Try to get the cached inverse
  if (!is.null(m)) {  # If the cached inverse exists
    message("getting cached data")  # Notify the user
    return(m)  # Return the cached inverse
  }
  data <- x$get()  # Get the matrix
  m <- solve(data, ...)  # Compute the inverse
  x$setInverse(m)  # Cache the computed inverse
  m  # Return the computed inverse
}
