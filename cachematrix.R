## These functions allow you to create a matrix wrapper that can cache its inverse 
## to save computation time. The first function, makeCacheMatrix, creates the wrapper object
## that can retrieve and set its value and inverse. The second function, cacheSolve, 
## returns the inverse of the matrix, using caching when applicable.

## Creates a wrapper for a matrix. Returns a list of four functions:
## set: takes a matrix as input, updates the value of the matrix
## get: no input, returns the value of the matrix
## setinv: takes a matrix as input, sets the cached inverse of the matrix
##              to the supplied value
## getinv: no input, returns the cached inverse of the matrix, NULL if 
##              the inverse has not been cached

## This function assumes the matrix is invertible.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <-- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)
}

## Return the inverse of a matrix stored as a CacheMatrix. 
## Retrieves the cached matrix if one is stored. 
## If none is stored, this function computes the inverse
## using solve(), and caches and returns the output.

## This function assumes the matrix is invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #attempt to retrieve cached inverse
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #cached data was null, so compute the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
