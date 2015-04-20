## These two functions allow the creation of a 
## cache-able matrix that can store the matrix inverse
## or calculate it if there is not one

## Creates a function that is a list of internal
## functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inversion of the cached matrix created in
## makeCacheMatrix() if there is one or calculates it
## otherwise. Assumes that x <- makeCacheMatrix()

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
        ## Return a matrix that is the inverse of 'x'

