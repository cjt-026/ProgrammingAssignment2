## the functions cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object, that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## initializing s to NULL means it is initialized to use later in the function
  set <- function(y) {
    x <<- y
    s <<- NULL
    ## set() here assigns the input argument (y) to the x object in the parent 
    ## environment, and clears any value of s cached by prior executions
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  ## '<<-' assigns the value to the right to the object in the parent environment
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## this function computes the inverse of the special "matrix" object that 
## is returned by makeCacheMatrix (recalls the inverse for the object passed as
## an argument)

cacheSolve <- function(x, ...) {
      
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
    ## if checks if the result is NULL - if s is not equal to NULL we have a
    ## valid cached mean and can return it
  }
  data <- x$get()
  s <- solve(data, ...) 
  x$setsolve(s)
  s
  ## if !is.null is false, cachesolve() calculates the inverse, uses setsolve() 
  ## to set the inverse, then returns it to the parent environment by printing s
}
