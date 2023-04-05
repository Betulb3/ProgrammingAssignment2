## Here are two functions named 'makeCacheMatrix' and 'cacheSolve'.
## "cacheSolve" function is used to take inverse of square matrix returned by "makeCacheMatrix". 


## makeCacheMatrix creates a square "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           # initialize inverse as Null
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x  # function that gets matrix x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv  # gets the inverse
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
      ## if inverse has already been calculated
    if(!is.null(inv)){   
        ## get the inverse from the cache and skips the calculation
        message("getting cached data")
        return(inv)
    }
    ## otherwise calculates the inverse
    data <- x$get()
    inv <- solve(data, ...)
    ## sets the value of the inverse by setInverse function
    x$setInverse(inv)
    inv 
}
