## The pair of functions makeCacheMatrix and cacheSolve work together
## to cache the inverse of a matrix so as not to waste computational
## resources by calculating it repeatedly

## makeCacheMatrix takes a matrix as input and returns a list of functions
## that can be used to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
              x <<- y
              m <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) m <<- inv
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve takes as input the output from the makeCacheMatrix function.
## cacheSolve returns the inverse of the matrix supplied by makeCacheMatrix
## either by returning a cached value of the inverse previously calculated
## or by calculating the inverse and the caching it for future use if needed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
