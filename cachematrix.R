## Function to store inverse of matrix in cache

## makeCacheMatrix Function to store inverse matrix with getter and setter functions

makeCacheMatrix <- function(x = matrix()) {
  # initialize null matrix
  invMat = NULL
  # setter function
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  # getter function
  get <- function() x
  # get inverse function
  getInv <-function() invMat
  # set inverse function
  setInv <- function(inv) invMat <<- inv
  list(set = set, get = get,getInv=getInv,setInv=setInv)
}


## Cache function to reuse inverse matrix information if available

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  # If inverse matrix is stored, use from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Else, get data, compute inverse matrix and store to cache
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}

