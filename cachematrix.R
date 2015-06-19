## makeCacheMatrix and cacheSolve are functions that can be used together 
## to store a matrix and its inverse and recall the cached inverse 
## instead of recalculating it



## makeCacheMatrix sets/sets a matrix and gets/sets its inverse

makeCacheMatrix <- function(x = matrix()) {
  get <- function() x
  set <- function(y) {
    x <<- y 
    i <<- NULL
  }
  setinverse <-function(solve) i <<-solve
  getinverse <- function() i
  list(get = get, set = set, setinverse = setinverse, getinverse = getinverse)
}




## cacheSolve returns the cached inverse of a matrix or calculates the inverse of a 
## matrix if the inverse is not already stored 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  inv <- x$get()
  i <- solve(inv)
  x$setinverse(i)
  i
}