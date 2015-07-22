##      These functions create a "matrix" object that can cache it's own inverse and solve/save
##      the inverse to cache in an object. The matrix object is a list containing a
##      function to set the value of the matrix, get the value of the matrix, set the value of
##      the inverse, and get the value of the inverse.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##      cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##      will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
