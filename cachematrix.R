
# Two functions, makeCacheMatrix and cacheSolve, enable us to cache & retrieve the inverse of a matrix. 
# They demonstrate ways to reduce potential time-consuming computations. Inverting a matrix
# can be very computation intensive especially if it is calculated repeatedly, like 
# in a loop. 

## This function creates a matrix object which can cache its inverse. It is really a list containing a function
## to set or get a matrix, and to set or get its inverse. 

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y){
       x <<- y
       i <<- NULL
   }
   get <- function() x
   setInverse <- function(inverse) i <<- inverse
   getInverse <- function() i
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
