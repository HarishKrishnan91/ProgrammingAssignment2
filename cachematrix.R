

## This function will help us in creating a special matrix which can cache its inverse by itself.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        set <- function(y) 
        {
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




## This function will compute the inverse of special matrix and will retrieve if the computation had been done before.

cacheSolve <- function(x, ...) {
inv <- x$getInverse()
        if (!is.null(inv)) {
                message("accessing cached data")
                return(inv)
                }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}}
