

## makeCacheMatrix manages caching of an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function above
## If the cached inverse is available, cacheSolve retrieves it, 
## if not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    } else {
        inv <- solve(x$get())
        x$setInverse(inv)
        return(inv)
    }
}
