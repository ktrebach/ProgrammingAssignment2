## The following contains a pair of functions that will either cache the inverse of a matrix
## if the inverse has previously been calculated or compute the inverse if it hasn't.

##The first function creates a special vector that sets the value of the matrix, gets the value of the matrix, 
##sets the value of the inverse of the matrix, and gets the value of the inverse of the matrix.

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


## The second function, returns the inverse, pulling the cached inverse if it has previously
##been calculated or computing it if it hasn't

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
            m <- x$getinverse()
            if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
      }

