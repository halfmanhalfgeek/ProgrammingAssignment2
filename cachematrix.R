## Set of functions that can cache objects that can be expensive to calculate from the original matrix provided
## Limitations:
##    Matrix must be square and invertable
##    Exceptions generated calculating the inverse are bubbled up to the caller
## Usage:
##    # Wrap the matrix in the object that supports caching
##    cacheable <- makeCacheMatrix(x)
##    # Get inverse, may calculate the inverse or return the previously calculated inverse
##    inverse <- cacheSolve(cacheable)

## Create a special matrix that can cache its inverse, when required
## Inverse is created on request, not up front
makeCacheMatrix <- function(x = matrix()) {
  # Initialise cache of inverse
  inverse <- NULL
  
  # Setter function; store the provided matrix internally
  set <- function(y) {
    # Store the matrix in x
    x <<- y
    ## Reset inverse (will not be valid any more)
    inverse <<- NULL
  }
  
  # Getter function; return the matrix
  get <- function() x
  
  # Store the inverse in the cach 
  setinverse <- function(inv) inverse <<- inv
  
  ## Get the cached inverse
  getinverse <- function() inverse
  
  # Return the list of methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  # Get the inverse from the cache - may not be set yet
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    # Not null - we've calculated it before - return it
    message("getting cached data")
    return(inverse)
  }
  # Otherwise, not calculated yet. Calculate it and store
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  # Then return the calculated inverse
  inverse
}
