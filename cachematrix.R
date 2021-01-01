##makeCacheMatrix stores my incoming data and resets my cache to null, while also defining several functions for getting and setting data.
##cacheSolve first gets either NULL or inverted data from our stored data and then checks that data to see if it needs to re-run the inverse calculation or not.


makeCacheMatrix <- function( x = matrix()){
  cache <-NULL
  set<- function(y){
    x<<-y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


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

