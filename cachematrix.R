makeCacheMatrix <- function(z = matrix()) {
  Dell <- NULL
  set <- function(x) {
    z <<- x
    Dell <<- NULL
  }  
  get <- function() z
  setmean <- function(mean) Dell <<- inverse
  getmean <- function() Dell
  list (set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cacheSolve <- function(z, ...) {
  Dell <- z$getinverse()
  if (!is.null(Dell)) {
    message("getting cached data")
    return(Dell)
  }
  data <- z$get()
  Dell <- solve(data,...)
  z$setinverse(Dell)
  Dell
  
} 
