## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function create a matrix with cache capabilities for the 'mean' operation
makeCacheMatrix <- function(x = matrix()) {
    ## the variable m is the cache
    m <- NULL
    ## the overload function 'set' will also clean the cache
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## the 'get' operation returns the 'x' in function scope
    get <- function() x
    ## the 'setinverse' operation plances the calculated mean value in cache variable
    setinverse <- function(mean) m <<- mean
    ## the 'getinverse' operation returns the cached mean value
    getinverse <- function() m
    ## at least, we return the matrix object (encapsulated) with the overloaded operations
    list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
## the cacheSolve function handle the cache information from the matrices created 
## by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## get the cached information
    m <- x$getinverse()
    if(!is.null(m)) {
        # if cached information is not NULL, we do not need to compute again
        message("getting cached data")
        # returns the cached inverse operation
        return(m)
    }
    ## it the execution get's here, we do not have the cached value
    ## so we get the data to perform the solve operation
    data <- x$get()
    ## we calc the inverse using the data from our special 'matrix' and store in 'm'
    m <- solve(data, ...)
    ## we save the result for further 'solve' operations
    x$setinverse(m)
    ## returns the calculated operation, the 'solve' result.
    m
}
