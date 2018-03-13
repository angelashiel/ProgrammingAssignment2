## Below are two functions that are used to create a special object
## that stores a square invertible matrix and cache's its inverse.

## This function creates a special "matrix" object that can cache its inverse.
## It is really a list containing a function to
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse
## (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) iv <<- inverse
        getinverse <- function() iv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        iv <- x$getinverse()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setinverse(iv)
        iv
}


