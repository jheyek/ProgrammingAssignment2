## The two functions defined here are a programming assignment for the Johns Hopkins 
## R Programming course offered via Coursera.
## The function below creates a list containing functions to store a matrix and 
## cache it's inverse. The matrix inverse can then be retrieved from cache if the
## original matrix has not changed.
makeCacheMatrix <- function(x = matrix()) {
  m = matrix()
  set <- function(y){
## The following if statement checks if a matrix being set is identical in size and 
## content to the existing matrix. If so, there is no need to clear the cache.
      if (is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y) ) {
        message("An idential matrix is already set.")
      }else{ ## input matrix is not identical, but let's check if it's at least a matrix
        if(is.matrix(y)){
          x <<- y
          m <<- NULL
        }else{
          message("Input argument to this fuction must be a matrix.")
        }
      }
  }
  get <- function() x
  setinverse <- function(invM) m <<- invM
  getinverse <- function() m
  list (set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}

## This function, if passed the list created by 'makeCacheMatrix' above, will return
## the matrix inverse, either by solving it or retrieving the cached result.
## If the function solves the matrix the inverse is then cached.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("returning cached matrix inverse")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat)
  x$setinverse(m)
  m
}
