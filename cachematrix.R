## The input matrix (x) is cached
## Its solved inverse is cached as NULL prior to any calculations

## Adapted the makeVector function to more applicable naming scheme
## 


makeCacheMatrix <- function(x =matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(Invert_matrix) inv <<- Invert_matrix
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## include an if-clause to determine whether a matrix has already been solved
## for its inverse, if so, the solution is withdrawn from cache instead of 
## reiterating its computation

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
  }
}