## The pair of functions below use R's scoping rules to cache
## the inverse of a matrix and then call this value instead of
## calculating it again, potentially saving computing time.

## The makeCacheMatrix function creates a list of functions that will
## "represent" a given matrix, with the possibility to (i) change the "stored"
## matix (set), (ii) retrieve the stored matrix (get), (iii) cache the inverse
## of the matrix (setinv) and (iv) provide this cached inverse (getinv)

makeCacheMatrix <- function(x = matrix()) {
      invm <- NULL
      set <- function(y) {
            x <<- y
            invm <<- NULL
      }
      get <- function() x
      setinv <- function(solv) invm <<- solv
      getinv <- function() invm
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## The cacheSolve function returns the inverse of a matrix "represented" by 
## a list created through makeCacheMatrix. If the inverse is in the "cache" of
## that list, it gets returned. If not, the inverse is calculated, cached and 
## returned.

cacheSolve <- function(x, ...) {
      invm <- x$getinv()
      if(!is.null(invm)) {
            message("getting cached data")
            return(invm)
      }
      data <- x$get()
      invm <- solve(data, ...)
      x$setinv(invm)
      invm    
}
