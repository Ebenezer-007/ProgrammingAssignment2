## Working on large vectors, matrices and datasets can be very time-consuming and require
## lots of computational power. This is especially important when the values in the object 
## e.g. matrix does not change. In such situations, it will be useful to be able to cache
## the results of computations done on these kinds of objects, so that they can easily be 
## retrieved instead of redoing costly computations, which is what the following two functions do.

# The makeCacheMatrix function creates a matrix object which can have its inverse cached.
# While it's basically a list, it contains a number of functions for manipulating the matrix

makeCacheMtrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse }
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# ThecacheSolve function, when called, initially computes the inverse of a given matrix  
# and caches it, returning the cached version of the matrix instead of recomputing 
# the inverse of the given matrix.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
