# Given Matrix inversion is usually a costly computation
# there is some benefit to caching the inverse of a matrix rather than computing it repeatedly
# makecachematrix creates an invertible matrix which is then cached. When called again the cached values are used
# if the matrix is the same as that cached

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

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# cachesolve returns the inverse of the created matrix and caches it for future reference if recalled

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("retrieving cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
