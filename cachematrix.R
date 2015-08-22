## makeCacheMatrix creates a special "matrix" that can cache its inverse
##list creates a list of the functions along with the value of matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Return a matrix that is the inverse of 'x'
## check if the inverse exists in the list, if yes then return the inverse
## from the cache. Otherwise call the fuction makeCacheMatrix to 
## calculate the inverse of the matrix

cacheSolve <- function(x,...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}