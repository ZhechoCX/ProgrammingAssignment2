## Write a pair of functions that cache the inverse of a matrix


## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {                          ## set the value of the matrix 
    x <<- y
    s <<- NULL
  }
  get <- function () x                          ## get the value of the matrix
  setinverse <- function (solve) s <<- solve    ## set the value of the inverse
  getinverse <- function() s                    ## get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## A function to compute the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()                           ## inverse of "x"
  if(!is.null(s)) {                             ## show operation in console
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse (s)
  s
}

