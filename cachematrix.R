## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m = matrix()
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(invM) m <<- invM
  getinverse <- function() m
  list (set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
