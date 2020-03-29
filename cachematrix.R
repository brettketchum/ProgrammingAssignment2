## This code allows the user to calculate the inverse of a matrix and store
## it in a cache.  If the previously calculated matrix inverse is needed later
## the user can obtain it from the cache instead of doing the complete
## calculation again.

## The function "makeCacheMatrix" takes an input matrix and allows the user to
## store the inverse of the matrix into a cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The function "cacheSolve" takes an input matrix and first checks to see if 
## the inverse has already been calculated and stored.  If so, it will return 
## the previously store inverse.  

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
