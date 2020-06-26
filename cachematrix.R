## Put comments here that give an overall description of what your
## functions do

## Creates CacheMatrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Solves for the Inverse of the Matrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
      message("Retrieving Cached Matrix")
      return(inv)
  }
  mat <- x$get()
  inv <- solve (mat, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
