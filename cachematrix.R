## Supports caching of matrix inverse.
## E.g.:
##   my_matrix <- makeCacheMatrix(matrix(rnorm(1000000), 1000, 1000))
##   system.time(inverse <- cacheSolve(my_matrix)) # slow
##   system.time(inverse <- cacheSolve(my_matrix)) # fast

## makeCacheMatrix: Builds a list object to cache a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() inv 
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of a caching matrix object returned by makeCacheMatrix.

cacheSolve <- function(m, ...) {
    inv <- m$getinv()
    if(!is.null(inv)) inv else m$setinv(solve(m$get()))
}