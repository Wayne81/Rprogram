## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Argument check
        if (class(x) != "matrix") {
                return("ERROR: Invaild argument x, x should be a matrix.")
        }

        ## creates a list containing 4 functions
        invMx <- NULL
        setMx <- function(y) {
                x <<- y
                invMx <<- NULL
        }
        getMx <- function() x
        setIn <- function(z) invMx <<- z
        getIn <- function() invMx 
        list(setMx = setMx,
             getMx = getMx,
             setIn = setIn,
             getIn = getIn)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' from cached data
        invMx <- x$getIn()
        if (!is.null(invMx)) {
                message("getting cached data")
                return(invMx)
        }

        ## solve the inverse of 'x' and cached the result for future
        invMx <- solve(x$getMx(), ...)
        if (is.matrix(invMx)) x$setIn(invMx)
        print(invMx)
}