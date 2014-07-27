## For large matrixes, the cost computation of the inverse of a matrix might be high.
## In these cases, it is useful to cache the inverse of the matrix, for the cases when it doesn't change over time.

## The example below first sets a variable 'mcm' to use the functions defined in makeCacheMatrix,
## then uses the 'set' function to cache the matrix,
## then calculates and caches its inverse by calling 'cacheSolve'.
## The second time it is called, it retrieves and displays the cached inverse.

## matrix1 <- makeCacheMatrix()
## matrix1$set(matrix(1:4, 2, 2))
## cacheSolve(matrix1)
## cacheSolve(matrix1)




## The makeCacheMatrix creates a list of functions (get, set, setinverse and getinverse)
## that is used to cache and retrieve the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matrix.inverse <- NULL
        set <- function(y) {
                x <<- y
                matrix.inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matrix.inverse <<- solve
        getinverse <- function() matrix.inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve retrieves the cached inverse of the matrix defined using 'makeCacheMatrix'.
## If it is not set, calculates and caches its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix.inverse <- x$getinverse()
        if(!is.null(matrix.inverse)) {
                message("getting cached data")
                return(matrix.inverse)
        }
        data <- x$get()
        matrix.inverse <- solve(data, ...)
        x$setinverse(matrix.inverse)
        matrix.inverse
}
