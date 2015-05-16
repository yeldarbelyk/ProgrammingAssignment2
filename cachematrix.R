## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly
## This will hopefully be an implementation of a couple of functions that do 
## this for us

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(get = get, set = set, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function checks to see if we've already done the inverse calculation
## and if we did it returns that. Otherwise, it performs the calculation
## and caches it. 
cacheSolve <- function(x, ...) {
        ## Check to see if we've cached previously, if so, return it
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Otherwise, get the data, perform the calculation
        ## set the inverse in the cache and return the inverse
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

