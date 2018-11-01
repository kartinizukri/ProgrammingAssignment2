## This function creates a special "matrix" object that can cache its inverse

## Initialize the inverse property
## Method to set the matrix
## Method the get the matrix
## Method to set the inverse of the matrix
## Method to get the inverse of the matrix
## Return a list of the methods

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
## Just return the inverse if its already set
## Get the matrix from our object
## Calculate the inverse using matrix multiplication
## Set the inverse to the object
## Return the matrix

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data) %*% data
  x$setInverse(m)
  m             
}
