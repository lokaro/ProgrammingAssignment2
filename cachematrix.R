## Put comments here that give an overall description of what your functions do

## makeCacheMatrix - caching solve function
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the value of the matrix to NULL
  m <- NULL
  ## declare set function 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## gets value
  get <- function() x
  ## calculate solve function
  setsolve <- function(solve) m <<- solve
  ## gets result of solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## check and set cache solve
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  ## return if exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if not cache
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
