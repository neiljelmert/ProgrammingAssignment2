## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(M = matrix()) {
      inv <- NULL
      set <- function(y) {
            M <<- y
            inv <<- NULL
      }
      get <- function() M
      setInv <- function(inverse) inv <<- inverse
      getInv <- function() inv
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(M, ...) {
      inv <- M$getInv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      matrix <- M$get()
      inv <- solve(matrix, ...)
      M$setInv(inv)
      inv
}
