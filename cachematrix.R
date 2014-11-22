## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    # Set Matrix Value - Accessor Method
    set <- function(y) {                  
        x <<- y
        m <<- NULL
    }
    # Get Matrix Value - Accessor Method
    get <- function() x
    # Set Solve Value - Accessor Method
    setsolve <- function(solve) s <<- solve
    # Get Solve Value - Accessor Method
    getsolve <- function() s
    # Function's Return Value
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    # Check if cache value already exists in makeCacheMatrix 
    # Object.
    s <- x$getsolve()
    if(!is.null(s)) {   # Cache Value exists -> return
      message("getting cached data")
      return(s)
    }
    # If cache value doesn't exist, then calculate "solve" value 
    # and cache it.
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    # Return Value
    s  
}
