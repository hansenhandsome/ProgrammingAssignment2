## Write A pair of functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "vector", which is  a list containing a function to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x)) stop("invalie matrix")
  d<-dim(x)
  if (d[1] != d[2]) stop ("invalie matrix")
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}

x=matrix(rnorm(16), 4, 4); x
y<-makeCacheMatrix(x)
z<-cacheSolve(y)
round(x%*%z,4)
