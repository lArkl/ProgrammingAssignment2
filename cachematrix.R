## Put comments here that give an overall description of what your
## functions do

# Creates a list containing functions to set a matrix cache, get it's value
# and get it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # Set and get functions of the matrix
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x

        # Set and get functions of the inverse
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        
        # Returns the list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# Gets the inverse of the cached matrix if it was computed,
# if not, it computes it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        # Already computed?
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        # gets the original matrix
        data <- x$get()
        
        # calls the cache function of inverse
        inv <- solve(data, ...)
        x$setinv(inv)
        
        # returns inverse
        inv
}