
makeCacheMatrix <- function(x = matrix()) {  ##create a function that set x as a Matrix
  inv <- NULL   ##set inv default value as NULL
  set <- function(y) {
    x <<- y     ##create a set function and assign the new matrix to the parent environment
    inv <<- NULL
  }
  get <- function() {x}  ##create a get function and return the value of x
  setinv <- function(inverse) {inv <<- inverse} ##assign inverse to parent environment
  getinv <- function() {inv} ##get inverse when called
  list(set = set, get = get,  ## create a list so you can subset using $
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {   ##create a function that will hold and return inverse of x
  inv <- x$getinv() 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) ##create inverse using Solve function
  x$setinv(inv)
  inv ##return inverse of x
}

