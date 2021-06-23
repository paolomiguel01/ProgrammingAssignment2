## In order to get the inverse of matrix, you need to follow this procedure

makeCacheMatrix <- function(z = matrix()) {
  inv <- NULL
  set <- function(x) {
    z <<- x
    inv <<- NULL
  }  
  get <- function() z
  setinverse <- function(mean) inv <<- inverse
  getinverse <- function() inv
  list (set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## To compute the value of the inverse of the matrix returned by the makeCacheMatrix above

cacheSolve <- function(z, ...) {
  inv <- z$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- z$get()
  inv <- solve(data,...)
  z$setinverse(inv)
  inv  
} 
