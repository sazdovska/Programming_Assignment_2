## These functions create matrix object and then computes 
## and cache its inverse, whcih then can be retrieved from
## the cache if had been already computed

## This function creates an object of class "matrix" that can 
## cache itself

makeCacheMatrix <- makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- solve(x)
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This functions computes the inverse of the matrix
## returned with makecacheMatrix() function, but If the inverse
## of the given matrix is already computed it retrives it
## from the cache

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
