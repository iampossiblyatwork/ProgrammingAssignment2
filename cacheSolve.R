cacheSolve <- function(x, ...){
  cache <- x$getinverse()
  if(!is.null(cache)){
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data,...)
  x$setinverse(cache)
  cache
}
