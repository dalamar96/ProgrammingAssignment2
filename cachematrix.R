
## These functions allow the creatoin of a smepcial "matrix" object that can have it's inverse
## calculated and then cached, so the next time the inverse is used it can be retrieved from cache
## instead of computing it again (which in some cases could take a long time). This will speed subsequent
## calls for the inverse of the "matrix" created as a makeCacheMatrix object

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m<<-solve
  getMatrix <- function() m
  list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getMatrix()
  if(!is.null(m)) {
    message("using cashed inverse!")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix,...)
  x$setMatrix(m)
  m
}