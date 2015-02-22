## Functions to create object to store matrices and retrieve from the cache.

## Creates an object with getters and setters methods to 
## get and set matrix and inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
	setmat <- function(xy) { 
                       x<<-xy
                       invmat<<-NULL
        }
	getmat <- function() x
	setinvmat <- function(yx) invmat<<-yx
	getinvmat <- function() invmat
	list(setmat = setmat, getmat = getmat, setinvmat=setinvmat, getinvmat=getinvmat)
}


## Retrieve from cache if already the inverse is calculated or else calculate, cache and return.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$getmat()
        m <- solve(mat)
        x$setinvmat(m)
        m
}
