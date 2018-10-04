## The purpose of the functions in this file is to speed calculations 
## by calculating a matrix inverse and saving it on "cache"
## The code creates the "cache" by taking advantage of R's lexical scoping


## makeCacheMatrix takes a matrix and initializes its inverse to the NULL value
makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL
    set <- function(y) {
        x <<- y
        xi <<- NULL
        
    }
    get <- function() x
    setInverse <- function(inverse) xi <<- inverse
    getInverse <- function() xi
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve takes an object created by makeCacheMatrix
## and returns the inverse of the matrix contained in the object; if the inverse doesn't already
## exist it is created otherwise the cached version is returned

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    xi <- x$getInverse()
    if(!is.null(xi)) {
        message("getting cached data")
        return(xi)
    }
    data <- x$get()
    xi <- solve(data)
    x$setInverse(xi)
    xi
}
