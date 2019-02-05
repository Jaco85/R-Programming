
# This first function (makeCacheMatrix) creates a special matrix object that can cache the inverse values 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function calculates the inverse of the special matrix created with the above 
# function. However, it first checks to see if the inverse has already been calculated. If so, 
# it gets the inverse matrix from the cache and skips the computation. Otherwise, it calculates 
# the inverse of the data and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#create matrix for testing
mtx <- matrix(c(1:4),2,2)

#Inverse the matrix for the first time
mtx_inv <- makeCacheMatrix(mtx)
cacheSolve(mtx_inv) 

#Once again from cache
cacheSolve(mtx_inv) 


