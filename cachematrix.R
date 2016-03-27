## The following functions allow for the caching of the inverse of
## a matrix, since this is an expensive operation. This is achieved 
## by taking advantage of the scoping rules of R.

## This function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inverse) {
    x_inverse <<- inverse
  }
  
  get_inverse <- function() {
    x_inverse
  }
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  
  inverse <- solve(data, ...)
  
  x$set_inverse(inverse)
  
  inverse
}
