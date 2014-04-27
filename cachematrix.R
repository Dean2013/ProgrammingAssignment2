## makeCacheMatrix(x) and cacheSolve(x) together find the inverse of a matrix.
## The fist time a matrix is entered as the argument, the inverse is 
## computed and the result cached and returned.  All later times the
## same matrix is entered as the argument to cacheSolve(), the cached
## inverse is returned

## makeCacheMatrix(x) creates a special "matrix" which indicates whether
## the inverse of x has already been found and, if so, caches it

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
## first checks to see if the inverse has already been calculated.  If so
## it just returns the already calculated inverse.  If not, it computes the
## inverse and stores it with the matrix.

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
