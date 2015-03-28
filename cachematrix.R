# Given Matrix inversion is usually a costly computation
# there is some benefit to caching the inverse of a matrix rather than computing it repeatedly
# makecachematrix creates a "matrix" object which is then cached. When called again the cached values are used
# if the matrix is the same as that cached

# NB: The supplied matrix must be invertible
# sample use
# > x = rbind(c(2, 4), c(4, 2))
# > m = makeCacheMatrix(x)
# > m$get()
#     [,1] [,2]
#[1,]    2    4
#[2,]    4    2
# >cachesolve(m)
#           [,1]       [,2]
#[1,] -0.1666667  0.3333333
#[2,]  0.3333333 -0.1666667

# if cachesolve(m) is run again with the matrix m unchanged it recalls the results from cache
# and prints "retrieving cached data"

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL # Inverse begins as NULL
  
  # Set the original object to be stored and later inverted
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Returns original object
  get <- function() x
  
  # Set and Get functions for inverse, similar to above
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function will retrieve the original matrix object, calculate its inverse,
## and cache the inverse in the CacheMatrix object.
## If the cache already exists, it will return the current inverse.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  # Check if cached inverse object already exists
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Get original matrix object
  data <- x$get()
  
  # Calculate inverse
  inv <- solve(data)
  
  # Cache inverse using CacheMatrix
  x$setInverse(inv)
  inv
}
