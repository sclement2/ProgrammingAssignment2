## The first function, makeCacheMatrix, creates a special "matrix" object that 
## can cache its inverse.
##
## Base assumption for using these functions is that the matrix supplied 
## is always invertible.
makeCacheMatrix <- function(x = numeric()) {
    
    ## initialize inverse matrix to null
    m <- NULL
    
    ## set the original matrix and clear the cached inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get the original matrix
    get <- function() x
    
    ## set the inverse matrix
    setinverse <- function(solve) m <<- solve
    ## get the inverse matrix
    getinverse <- function() m
    
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the "special" matrix returned
## by makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## If the inverse has already been calculated (and the matrix has not changed),
    ## then retrieve the inverse from the cache.
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## If the inverse has not been calculated then calculate and set the inverse
    ## in the cache. Return the inverse matrix of 'x'.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
