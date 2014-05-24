## The functions below are designed to work together to cache the inverse of
##  a given matrix. It is assumed that the matrix is invertable. 

## This function sets up a list of functions that cache 
##  the inverse of a matrix passed to it.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmtrx <- function(solve) m <<- solve
        getinvmtrx <- function() m
        list(set = set, get = get,
             setinvmtrx = setinvmtrx,
             getinvmtrx = getinvmtrx)
}


## This function first tests to see whether an inverted version
##  of the matrix passed to it is cached. If so, it returns this cached inverse matrix.
##  Otherwise, it calculates the inverse matrix and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinvmtrx()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmtrx(m)
        m
}
