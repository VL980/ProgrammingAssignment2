## Put comments here that give an overall description of what your
## functions do

## Produces a list of functions that can be used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <-  function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsol <- function(solve) m <<- solve
  getsol <- function() m
  
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)
}


## Returns a matrix that is the inverse of the matrix given in argument of previous 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsol()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsol(m)
  m
}
