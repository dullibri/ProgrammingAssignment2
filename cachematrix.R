## Put comments here that give an overall description of what your
## functions do

## This function creates a cache for a square matrix' inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
  )
}


## This function computes the inverse of a square matrix if it is not already
## in the chache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'if it not already in the cache
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
