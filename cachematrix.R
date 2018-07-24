## Caching the Inverse of a matrix
## The function below is used to create a special object which 
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
                  x <<- y
                  inv <<- NULL
         }
         get <- function() x
         setInverse <- function(inverse) inv <<- inverse
         getInverse <- function() inv
         list(set = set, 
              get = get, 
              setInverse = setInverse,
              getInverse = getInverse)
}

## The function below computes the inverse of a matrix created
## by the previous function.
## If the inverse has already been computed and cached, it 
## should return the cached data.

cacheSolve <- function(x, ...) {
         ## Returns a matrix that is the inverse of "x"
         inv <- x$getInverse()
         if(!is.null(inv)) {
                  message("getting cached data")
                  return(inv)
         }
         mat <- x$get()
         inv <- solve(mat, ...)
         x$setInverse(inv)
         inv
}
