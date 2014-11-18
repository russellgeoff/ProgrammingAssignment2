## These two functions create a special object that can store a matrix 
## and its inverse. Another function computes the inverse if needed.
## This allows the inverse matrix to be calculated once and be referenced 
## multiple times in the future.
## The cacheSolve function will invert the matrix or reference the cached 
## version.

## Create a special matrix object that can cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL ## Sets the inverse matrix to NULL to prevent old inverses from being returned
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the matrix stored in the special matrix object.
## Returns the cached version if the matrix has not changed.

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if (!is.null(inverse)){
    message('Getting cached inverted matrix')
    return(inverse)
  }
  
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinv(inverse)
  inverse
}
