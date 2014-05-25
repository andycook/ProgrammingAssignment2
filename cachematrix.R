## This file defines functions to manage certain aspects of a matrix,
## including the inverse, so that possibly expensive computation is only
## performed when needed, such as only recalculating the inverse if
## the matrix has been changed.

## makeCacheMatrix provides functions to get and set a matrix and a matrix
## representing its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  # This is the variable in which the cached inverse will be held
    set <- function(y) {
        x <<- y
        i <<- NULL  # Reset the inverse since the matrix has possibly changed
    }
    get <- function() x  # Just return the matrix
    # the cacheSolve function calls setinverse
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i  # Return the cached value stored in i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve sets the inverse matrix in the function passed to it.
## It also prints a message if it just gets the cached value from the
## passed function, and returns the inverse matrix, whether from the
## cached value, or newly computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
