## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMat = NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  getInv <-function() invMat
  setInv <- function(inv) invMat <<- inv
  list(set = set, get = get,getInv=getInv,setInv=setInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}

