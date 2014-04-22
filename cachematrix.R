## makeCacheMatrix function creates four functions: setting the matrix, gettig the matrix, 
## setting the inverse of the matrix and getting the inverse of the matrix

## Function needs matrix as an argument;
## Function returns list of 4 items and getinv() returns NULL
## so, e.g. c <- rbind(c(1, 2), c(1/3, 1/4))
##          d <- makeCacheMatrix(c)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL     # initialize stored inverse mtx to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The output of cacheSolve comes from 
## cache ("getting cached data" warning is then returned) OR computation.
## e.g. run: cacheSolve(d) to get inverse of the mtx

cacheSolve <- function(x, ...) {
    m <- x$getinv()           
    if(!is.null(m)) {    # if there is a cache already
      message("getting cached data") 
      return(m)          # return the cache without time-consuming computation needed
    }
    data <- x$get()      # if there is no cache
    m <- solve(data, ...)# compute the inverse of the matrix
    x$setinv(m)          # and store it into x's cache
    m
}

## to confirm, run:  d$get() %*% d$getinv() - identity mtx should be returned

## this is the example how <<- op. works - objects are "nested" within functions, not in the global env.
