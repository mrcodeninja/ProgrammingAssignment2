# Below are two functions that are used to create a special object that stores a matrix and
# caches its inverse.
#
# Example:
#   example.matrix <- matrix(c(4, 3, 3, 2), nrow=2, ncol=2)
#   cache.matrix <- makeCacheMatrix(example.matrix)
#   inverse.matrix <- cacheSolve(cache.matrix)  # Computes the inverse
#   inverse.matrix <- cacheSolve(cache.matrix)  # Returns the previously computed inverse


# Creates a special list that contains functions for caching matrices and associated inverse.
#
#   Args:
#       x:  The matrix for which to calculate the inverse.
#
#   Returns:
#       A special list containing the matrix, and an associated cached inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inv) {
        i <<- inv
    }
    
    getinverse <- function() {
        i
    }
    
    list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


# Return a matrix that is the inverse of 'x'.
#
#   Args:
#       x:  Special list containing the matrix, and cached inverse matrix.
#
#   Returns:
#       The inverse of matrix 'x'.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if (!is.null(i)) {
        message("Getting cached data.")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    
    i
}
