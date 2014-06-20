# Pair of functions to allow matrix solve results to be cached
# For example:
#
#> m<-makeCacheMatrix()
#> aMatrix <- matrix(c(1:4), 2,2)
#> m$set(aMatrix)
#> cacheSolve(m)
#> cacheSolve(m)
#
# In that example, solve will only be called once (by the first call to
#   cacheSolve.  The second call to cahceSolve will reuse the results from
#   the first call.

# Construct the cache object that can be used with cacheSolve to cache the results
#   of a call to solve
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Invert / solve the matrix stored in the cache object, using the cached
#   results if available
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
