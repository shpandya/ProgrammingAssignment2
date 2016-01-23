## The makeCacheMatrix function takes in a matrix and returns an object
## with methods to get/set the matrix, and get/set the inverse, while 
## keeping track of the staleness of the inverse.
## The cacheSolve function returns the inverse of the CacheMatrix object.

## Returns a list of functions set, get, setinverse and getinverse that
## manipulate a matrix.  Also tracks whether the matrix has changed or not
## and so, if the inverse is stale or not.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    inverseStale <- TRUE
    
    set <- function(y) {
        x <<- y
        i <<- NULL
        inverseStale <<- TRUE
    }
    
    get <- function() x
    
    setinverse <- function(inverse) {
        i <<- inverse
        inverseStale <<- FALSE
    }
    
    getinverse <- function() {
        if(inverseStale == TRUE) {
            NULL
        } else {
            i
        }
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Returns the inverse of the CacheMatrix object.  If already computed, returns from 
## the object, otherwise computes it and stores it in the object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    if(is.null(i)) {
        data <- x$get()
        i <- solve(data, ...)
        print("Caching inverse...")
        x$setinverse(i)
    }
    
    i
}
