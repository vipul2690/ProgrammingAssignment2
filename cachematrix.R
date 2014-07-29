## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix to be cached

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) m <<- solve
  
  getinverse <- function() s
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function creates a inverse and caches the value of inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  
  data <- x$get()
  s<- solve(data, ...)
  
  x$setinverse(s)
  
  s
}
