## This set of functions creates a matrix object that can cache its own inverse
## and will return the cached inverse if the inverse for a matrix has already been calculated
## instead of recalculating it.

## makeCacheMatrix creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<-y
    i<<-NULL
  }
  get <- function()x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cachesolve computes the inverse of the "matrix" returned by makeCacheMatrix or 
##returns the inverse if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matr <- x$get()
  i <- solve(matr,...)
  x$setinv(i)
  i

}
