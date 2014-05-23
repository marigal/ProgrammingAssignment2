## Put comments here that give an overall description of what your
#   makeCacheMatrix: creates a special "matrix", which is really a list containing a function to 
#1. set the values of the matrix
#2.	get the values of the matrix
#3.	set the values of the matrix inverse
#4.	get the values of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  
    set <- function(y) {
       x <<- y
       m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  
  # If the inverse is already calculated, return it
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If the inverse is not calculated, do it now
  m <- solve(x$get()) 
  
  # Cache the inverse
  x$setsolve(m)
    
  # Return it
  m
}
