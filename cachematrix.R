## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## These following functions only work with invertible matrices

## This first function is to create a special "matrix",
## which is a list containing a function to:
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse of the matrix
## 4- get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## This function calculates the inverse of the special "matrix",
## which was created via the makeCacheMatrix function.
## It always first checks to see if the inverse of the special "matrix" was
## already calculated. If so, it will get the inverse of the special "matrix"
## from the cache and skip the computation. Otherwise, it calculates the inverse
## of the special "matrix" and sets the value of the inverse of the special 
## "matrix" in the cache via the setinverse function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


