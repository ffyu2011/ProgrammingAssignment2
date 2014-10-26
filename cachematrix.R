## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special matrix, which is
## a list containing four functions:
## function set() is used to set the values of this matrix
## function get() is used to get the values of this matrix
## function setinverse() is used to set the values of the inverse of this matrix
## function getinverse() is used to get the values of the inverse of this matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## Function cacheSolve() calculates the inverse of the special matrix
## created with the function makeCacheMatrix()
## First, it checks whether the inverse has been calculated (and the matrix has not changed)
## If so, it gets the inverse from the cache and skip the computation
## Otehrwise, it calcualtes the inverse of the new special matrix, and sets the values
## of the inverse matrix in the cache via the setinverse() function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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