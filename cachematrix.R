## These functions simply enable calculating the inverse of multiple given matrices
## 

## makeCacheMatrix() function creates a matrix and extended functionalities for getting
## or setting the matrix

makeCacheMatrix <- function(x = matrix()) {
  my.matrix <- x
  inv <- NULL
  set <- function(y) {
    my.matrix <<- y
    inv <<- NULL
  }
  get <- function() my.matrix
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function gets the inverse of a matrix, if calculated before, or calculates the
## inverse and saves it to x$setinv

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  get.cached.inv <- length(x$getinv())
  if(get.cached.inv>0) {
    print('Getting cached inverse')
    x$getinv()
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}