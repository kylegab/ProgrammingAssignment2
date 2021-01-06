## Set the input x as a matrix
## and then set the solved value "n" as a null
## thenchange every reference to "mean" to "solve"
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) n <<- solve
  getsolve <- function() n
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
##
## Same here, changed "mean" to "solve" and "m" to "n"
cacheSolve <- function(x, ...) {
  n <- x$getsolve()
  if(!is.null(n)) {
    message("getting inversed matrix")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setsolve(n)
  n
}
