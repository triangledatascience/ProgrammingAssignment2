
## R Programming, Week 3, Programming Assignment 2
## The following functions are used to obtain the inverse of a given matrix
## efficiently by leveraging caching the inverse of the matrix.


## makeCacheMatrix is a function which returns a list of functions which 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
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


## cacheSolve is a function which returns the inverse of a given matrix. If the
## inverse is available in the cached data, the cacheSolve would return the 
## inverse in the cache. Otherwise, it would compute and return the inverse.

cacheSolve <- function(x, ...) {
    i = x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i = solve(data, ...)
    x$setInverse(i)
    i
}

