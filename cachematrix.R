## The following functions returns the inverse of a matrix. 
## If the inverse is there in the global environment, the value is returned.
## Otherwise, inverse of the matrix is calculated and returned.

## this function creates a special vector which contains function to:
## set the value of matrix
## get the value of matrix
## set the value of inverse
## get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}


## this function gets the inverse of the matrix from cache, if present
## and returns it. Otherwise, it computes the inverse and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}