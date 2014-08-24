## Take a matric and stores a cached copy of its inverse. It is computed the 
## first time; subsequently the stored copy is returned.

## Create a cached matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
     	 x <<- y
         m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## Finds the inverse of the matrix created with the above function. It first
## checks to see if the inverse has already been calculated. If so, it returns 
## the cached value. Otherwise, the invese is computed, cached and returned.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
