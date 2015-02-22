
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## this can be helpful in avoiding recalculating the inverse, which is a costly step
## we use the solve function to invert - assuming an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setsolve(m)
        m
}
