## The functions cache the inverse of a matrix. The first function creates the object (list) that
## can cache the inverse of the input matrix and then the second function calculates the inverse (using the object 
## created by the first function). If the inverse of the specific matrix has already been calculated it will 
## get the inverse from the cache, not calculating it again

##  Creates a "matrix" object that can cache its inverse (if the matrix is invertible)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}


## Returns the inverse of 'x' matrix created by makeCacheMatrix. If the inverse of the matrix 
## has already been calculated, then it retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
