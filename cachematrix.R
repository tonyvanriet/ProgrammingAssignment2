## Create a matrix that will cache it's inverse

## Creates a list with functions to get and set the
## matrix, and to get and set the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  set(m)
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the given cacheMatrix.
## Returns a cached version of the inverse if it's already
## been calculated.

cacheSolve <- function(x, ...) {

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
