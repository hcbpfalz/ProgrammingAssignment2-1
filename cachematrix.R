## Put comments here that give an overall description of what your
## functions do

## These two functions will cache and return matrices and their inverses
## to speed up processing times

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computtes the inverse of the special "matrix" returned by 'makeCacheMatrix' above.
## If the inverse has already been calculated (and matrix has not changed), then this function should 
## retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
    m <- x[getinverse()]
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x[get()]
    m <- solve(data, ...)
    x[setinverse(m)]
    m
}
