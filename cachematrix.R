## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) invm <<- inv
    getinverse <- function() invm
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
