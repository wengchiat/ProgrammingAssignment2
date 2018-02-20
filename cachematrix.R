## Put comments here that give an overall description of what your
## functions do

##Below are two functions that are used to create a special object that stores a numeric vector and caches its mean.
##
##The first function, makeVector creates a special "vector", which is really a list containing a function to
##
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setSolve(m)
        m
}
