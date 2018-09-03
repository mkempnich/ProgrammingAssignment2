## These are a pair of functions that can cache the inverse of a matrix.


## The first function creates a special 'matrix' object, able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inverse <<- solveMatrix
        getInverse <- function() inverse
        list( set = set, get = get, 
              setInverse = setInverse, 
              getInverse = getInverse)
}


## This second function computes the inverse of the special "matrix" returned by the first function.
## If the inverse has been calculated already - and the matrix remains unchanged - it
## retrieves that inverse from the cache. Otherwise, the inverse is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
