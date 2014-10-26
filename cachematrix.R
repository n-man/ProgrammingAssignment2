## makeCacheMatrix serves for creating cached matrix from
##      common matrix
## cacheSolve calculates inverse matrix if the result is not
##      cached already

## Creates cached matrix object
## Parameters:
##   X (optional) - input matrix
## Methods:
##   get - returns original matrix
##   set(Y) - set original matrix to Y
##   getSolve - get inverse matrix if it is already computed or NULL
##   setSolve(invResult) - get inverse matrix to invResult

makeCacheMatrix <- function(X = matrix()) {
    invM <- NULL
    get <- function() X
    set <- function(Y) {
        X <<- Y
        invM <<- NULL
    }
    getSolve <- function() invM
    setSolve <- function(invResult) invM <<- invResult
    list(set = set, get = get,
         getSolve = getSolve,
         setSolve = setSolve)
}


## Compute inverse matrix for cached matrix
## Parameters:
##   X - cached matrix
##   ... - other parameters for Solve method
## Returs inverse matrix of cached matrix. If the inverse
## matrix is already computed, returns cached result,
## otherwise the inverse matrix is computed.

cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'x'
    invM <- X$getSolve()
    if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
    }
    data <- X$get()
    invM <- solve(data, ...)
    X$setSolve(invM)
    invM
}
