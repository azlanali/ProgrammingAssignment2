## Put comments here that give an overall description of what your
## functions do
# Write the following functions: to make the computational faster
# makeCacheMatrix: 
# # cacheSolve: 





## Write a short comment describing this function
# first create the function similar to the sample function, assume matrix invertible
#   This function creates a special "matrix"  and using <<- to make the variable inside the function, 
# the will use the inverse function
# object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function (y){
        x <<- y
        inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getinverse <- function() {inv}
      list(set = set, get=get, setInverse = setInverse, getinverse = getinverse)
}


## Write a short comment describing this function
#   This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("trying to get cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
