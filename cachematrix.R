# makeCacheMatrix will create a special vector with four functions
# set which will set the matrix to a variable x
# get which will return the value of the variable x
# setinverse which will take the inversed martix and setit to variable m
# getinverse will return the cached inversed matrix

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
# CacheSolve will check if the value stored in m is null, 
# if it is null, it will calculate the inverse of matrix using solve function and
# make a call to setinverse function which will store the value in variable m again else
# it will returned the cached value

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
