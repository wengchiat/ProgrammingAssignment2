## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.
## it is assumed that the matrix supplied is always invertible.

## This function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##  -set the value of the matrix
##  -get the value of the matrix
##  -set the value of the inverse matrix
##  -get the value of the inverse matrix

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
             setSolve = setSolve,
             getSolve = getSolve)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse matrix in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat)
        x$setSolve(m)
        m
}
