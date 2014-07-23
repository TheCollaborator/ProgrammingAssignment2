## These functions (makeCacheMatrix and cacheSolve) are used to take in an invertible square matrix and make a list of it which also stores its inverse if calculated so that it can be found in the cache rather that recalculated later.

## Following Function (makeCacheMatrix takes in an inversible square matrix and sets it up as a special list with the inv of the matrix cached if it is computed and set.

makeCacheMatrix <- function(x = matrix()) {
      matinv <- NULL
      set <- function(y) {
            x <<- y
            matinv <<- NULL
      }
      get <- function() x
      setinv <- function(inv) matinv <<- inv
      getinv <- function() matinv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## Following function (cacheSolve) takes in the special list from makeCacheMatrix and checks the cache for an inverse if it has already been calculated or computes it and caches it if it has not.

cacheSolve <- function(x, ...) {
      matinv <- x$getinv()
      if(!is.null(matinv)) {
            message("getting cached data")
            return(matinv)
      }
      data <- x$get()
      matinv <- solve(data, ...)
      x$setinv(matinv)
      matinv
}
