## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function () inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## computes the inverse of the special "matrix" created by makeCacheMatrix
## first checks to see if the inverse has already been calculated, if so
## it just returns the already calculated inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    ## if the inverse is cached, just return the cached inverse
    if(!is.null(inv)) {
        message("getting cashed data")
        return(inv)
    }
    data <- x$get()
    ## compute the inverse
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
