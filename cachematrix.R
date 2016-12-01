## These functions create a matrix and solves for the given matrix's inverse.
## (The original matrix must be a square, invertible matrix in order to get 
## solve its inverse matrix.) The inverse matrix is cached so the computer does
## not have to recompute the inverse each time. But, if the original matrix is 
## changed, the inverse matrix will be reset to null, and will be recomputed.
## The first function serves to set and get the values of the matrix, and set 
## and get the values of the inverse matrix. The second function serves to 
## either retrieve the cached inverse matrix or calculate the inverse matrix if
## it has not yet been computed.

## makeCacheMatrix creates a special matrix that sets the value of the matrix, 
## gets the value of the matrix, sets the value of the inverse matrix, and gets 
## the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix in makeCacheMatrix. If the 
## inverse has already been calculated (and the matrix hasn't changed), then 
## cacheSolve will retrieve the inverse that is already cached, instead of
## calculating it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
            message ("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
