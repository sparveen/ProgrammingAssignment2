## function makeCacheMatrix creates a special matrix that cache the inverse of a matrix
## functions cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix

## makeCacheMatrix creates a special matrix
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinversematrix <- function(solve) i <<- solve
    getinversematrix <- function() i
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)     
}


## cacheSolve caches the inverse of a matrix
## checks if the inverse is computed
## if inverse computed, caches it and skips computation
## Else, it computes the inverse of the matrix and 
##sets the value of the inverse in the cache via the setinversematrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinversematrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversematrix(i)
  i    
}
