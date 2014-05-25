## These functions invert square matrices and provide functionality to cache
## the result for use in later calculations

## this function stores the matrix to e inverted as well as 
## the inverted matrix after the calculation has been 
## completed.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## this function accepts the makeCacheMatrix function as an arguement and returns the inverted matrix of the matrix
## supplied in the makeCacheMatrix function. If the inverted matrix already exists in the cache, the function will 
## return the answer from the cache, otherwise it will calculate the inverted matrix and then set it in the cache
## for use in later operations

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m  ## Return a matrix that is the inverse of 'x'
}
