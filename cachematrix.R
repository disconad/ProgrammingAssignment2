## Two functions to calculate and then cache a matrix and its inverse.

## makeCacheMatrix creates a special "matrix" to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverese of x. 
## First it determines if there is a cached value for the inverse of x in the existing matrix.
## If so, the inverse is returned.
## If not, solve is used to get the inverse and sets the cache value for future use. The inverse is returned.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m) 
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
