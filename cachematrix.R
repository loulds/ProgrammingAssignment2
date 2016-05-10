## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(sx = matrix()) {
        sm <- NULL
        set <- function(sy) {
                sx <<- sy
                sm <<- NULL
        }
        get <- function() sx
        setinverse <- function(solve) sm <<- solve
        getinverse <- function() sm
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}
cacheSolve <- function(sx, ...) {
        ## Return a matrix that is the inverse of 'x'
        sm <- sx$getinverse()
        if (!is.null(sm)) {
                message("getting cached data")
                return(sm)
        }
        data <- sx$get()
        sm <- solve(data, ...)
        sx$setinverse(sm)
        sm
}
