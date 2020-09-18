##There are two functions. They compute and cache the inverse of a matrix so that
## it does not need to be re-calculated repeatedly

##This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inverse}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function retrieves the inverse matrix from cache (if it's there and the matrix
## has not changed), otherwise it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
