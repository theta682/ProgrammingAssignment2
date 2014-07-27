## This module provides a mechanism to cache results of a matrix inverting

## makeCacheMatrix function add caching to a matrix

makeCacheMatrix <- function(m = matrix()) {
  im <- NULL
  set <- function(y) {
    m <<- y
    im <<- NULL
  }
  get <- function() m
  setinv <- function(y) im <<- y
  getinv <- function() im
  cacheSolve <- function() {
    if(!is.null(im)) return(im)
    im <<- solve(m)
    return(im)
  }
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv,
       cacheSolve = cacheSolve)
}

## cacheSolve function is used to get inverted matrix using hidden method
## of object returned by makeCacheMatrix

cacheSolve <- function(m, ...) m$cacheSolve()
