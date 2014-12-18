## Lazy evaluation of a matrix inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL # initialize cache to nothing
  
  # Re-initializes the "matrix" object so we can re-use it after it has been created.
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  
  # Returns the data matrix stored in the object.
  get <- function() x
  
  # Sets the value of the cached inverse.
  setinv <- function(inv) iv <<- inv
  
  # Returns the cached inverse value (NULL if not cached).
  getinv <- function() iv
  
  # return a list of inner functions defined above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" constructed by the makeCacheMatrix function defined above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data") # give some feedback :-)
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv # Return a matrix that is the inverse of 'x'
}
