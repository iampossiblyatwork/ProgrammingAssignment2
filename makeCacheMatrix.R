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