## Put comments here that give an overall description of what your
## functions do
##--------------------------------------------------------------
## The makeCacheMatrix function takes a square matrix as input,
## and creates makeCacheMatrix object that saves state of 
## an input matrix and its inverted value. Access methods are 
## provided to set/get the input matrix as well as the computed
## inverse matrix.                                       . 
## The cacheSolve function is used to compute the inverse
## of the input matrix, and provide caching of previously
## computed values that haven't changed. 
##--------------------------------------------------------------

##--------------------------------------------------------------
## makeCacheMatrix:
## This function takes a square matrix as input. It maintains
## an image of the input matrix and an image for an inverted
## value of the input matrix. Two functions,  set and get, 
## are defined to set and get the input matrix.
## Two functions, setSolve and getSolve, are defined to 
## set and get the cached inverse matrix result. 
##--------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve<- function(input) m <<- input  
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
##--------------------------------------------------------------
## cacheSolve:
## This function takes a makeCacheMatrix object as input.
## It will return the inverse of a square matrix, created via 
## makeCacheMatrix function, and cache the result such that 
## the cached value will be returned if the input matrix has
## not changed.
##--------------------------------------------------------------


cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
