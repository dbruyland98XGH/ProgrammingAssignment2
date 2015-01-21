## makeCacheMatrix and cacheSolve are 2 functions that work together in the same 
## way as the example functions makeVector and CacheMean provided to assignment2
##
## They exploit the function environment concept to cache an inversed matrix
## And provide the cached inversed matrix when it is available, otherwise it is
## calculated and cached (first time)


##
## MakeCacheMatrix 
## expects as argument a square matrix and returns a list of functions 
## that share a prepared function environment for this square matrix.
## A square matrix has same number of rows and columns
## 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setIM <- function(im) m <<- im
  getIM <- function() m
  list(set = set, get = get,
       setIM = setIM,
       getIM = getIM)
}

##
## cacheSolve
## expects a list of functions that share a prepared function environment
## related to a square matrix
## this list of functions was obtained as a result of a call to MakeCacheMatrix
## with the aformentioned square matrix
## chachesolve returns the inverse of the aforementioned square matrix.
## For the first call of cachesolve with the list, this is a calculated inverse
## For subsequent calls this is a cached inverse matrix.
##
cacheSolve <- function(x, ...) {
  m <- x$getIM()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setIM(m)
  m
}


## test
##> xm<-matrix(1:4,nrow=2,ncol=2)
##> ym <- makeCacheMatrix(xm)
##> zm <- cacheSolve(ym)
##> um <- xm %*% zm
##> um
##     [,1] [,2]
##[1,]    1    0
##[2,]    0    1
##> zmm <- cacheSolve(ym)
##getting cached data
##> um <- xm %*% zm
##> um
##     [,1] [,2]
##[1,]    1    0
##[2,]    0    1


