## Put comments here that give an overall description of what your
## functions do

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Set the value of makecachematrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the value of makecachematrix
  get <- function() x
  ## set inverse of the same matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## get the value of inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  ## checking whether inverse is calculated or not
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## get the matrix value
  mat <- x$get()
  ## calculate the inverse of matrix
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
