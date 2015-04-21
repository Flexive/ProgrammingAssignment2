## This "package" defines an "object type" CacheMatrix which is matrix
## with caching inverse calculation optimization. It provides functions
## to create such matrix from usual matrix and calculate inverse with caching.

## Creating CacheMatrix object from a given matrix x which is able to 
## cache inverse matrix.
## Parameters:
## x    --- given matrix.
## Variables:
## x    --- represents the matrix.
## inv  --- represents the inverse matrix.
## Functions:
## set/get functions for the above variables.

makeCacheMatrix <- function(x = matrix()) {

    #Setting up basic set/get functions.

    inv  <- NULL
    set    <- function(y) {
        x   <<- y
        inv <<- NULL #forgetting outdated inverse.
    }
    get    <- function() {
        x
    }
    setinv <- function(inverse) {
        inv <<- inverse
    }
    getinv <- function() {
        inv
    }

    #Creating CacheMatrix object as a list of the above functions.

    list(set    = set, 
         get    = get,
         setinv = setinv,
         getinv = getinv)

}

## Calculating inverse matrix of the CacheMatrix object and caches it.
## If it's already calculated returns cached value.
## Parameters:
## x    --- CacheMatrix object.
## safe --- if true, cached value is checked for validity before return
## [in case there is possibility of direct access to matrix of x through,
## for example, environment(x$get)$x = ...].
## ...  --- parameters for solve function to find inverse.

cacheSolve <- function(x, safe = FALSE, ...) {
    
    #Getting matrix and its possible inverse.

    inv  <- x$getinv()
    xnow <- x$get()

    #Checking validity of cached inverse, and return it if it is valid.

    if (!is.null(inv)) {

        #Performing check and returning cached value.

        if (!safe | all.equal(xnow %*% inv, diag(nrow(xnow)))) {
            message('Returning cached data.')
            return(inv)
        } else {
            message('Cached data is outdated: recalculating.')
        }

    }

    #Recalulating inverse matrix and caching it.

    inv <- solve(xnow, ...)
    x$setinv(inv)
    inv
}