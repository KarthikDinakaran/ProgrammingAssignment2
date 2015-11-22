## These are functions that would create a special wrapper around the matrix data
## structure and include a caching logic as part of the wrapper code.
## 

## This method takes an existing matrix object as argument and returns a wrapper 
## around the matrix object. The wrapper is essentially a list of "handlers" that 
## can be used to access the matrix object inside it, and also access the cached 
## inverse. This interface/handlers are used by the cacheSolve function below to 
## calculate inverse and also update the cached value in it.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function takes the matrix wrapper created by the makeCacheMatrix function 
## and returns the cached value of inverse if it exist, and if there is no cached
## value, it will calculate the inverse, update the cache as well as return it.

cacheSolve <- function(x, ...) {
        slv <- x$getSolve()
        if(!is.null(slv)) {
                message("getting cached data")
                return(slv)
        }
        data <- x$get()
        slv <- solve(data, ...)
        x$setSolve(slv)
        slv
}
