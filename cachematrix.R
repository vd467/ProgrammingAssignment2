## cacheMatrix provides 2 functions to cachke the inverse of a matrix 
## Once, it is cached, the next time you try to inverse teh same matrix, 
## it will return the value from the cache.

## This function creates a cache of a matrix in another environment.
## the value cached could be anything - does not have to be an inverse.
makeCacheMatrix <- function(x = matrix()) {
    # set a variable that will hold the inverse
    m <- NULL
    
    # generate the set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # the get function
    get <- function() x
    
    # setting of inverse function.
    setinverse <- function(solve) m <<- solve
    
    # get the cached inverse
    getinverse <- function() m
    
    # returns a list with each list tag having the same
    # name as the function and the value of the tag is the
    # function
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## this function returns a cached inverse of the matrix.
## if the inverse is not available in the cache, it will
## creae the inverse, cache it and return it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse
    
    # if inverse if found, m will not be null
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # get the value of X
    data <- x$get()
    
    # inverse it
    m <- solve(data, ...)
    
    # cache the inverse
    x$setinverse(m)
    
    # return the inverse
    m
}
