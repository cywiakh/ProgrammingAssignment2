## Function makeCacheMatrix:
## This function creates an object which itself contains other
## functions. These functions are only used as accessor methods
## for x (the matrix) and s (the "solve" value pre-calculated for x)
## The return value is a list with all 4 functions:
##     - set(x)
##     - get()
##     - setsolve(solve)
##     - getsolve()

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


## Function cacheSolve:
## This function applies the solve() function to the cached matrix.
## It essentially checks if a "solve" value has already been 
## calculated for that object. If it has, it returns the value
## otherwise, it calculates and stores it for future use.

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
