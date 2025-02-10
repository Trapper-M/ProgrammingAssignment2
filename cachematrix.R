
## Creates an object that contains a matrix (x) and can store its inverse (i)
## provides functions to set and get the matrix data
## and to set and get the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Returns the inverse of a matrix object created with makeCacheMatrix
## The cached value will be returned if it exists,
## if not, the value is calculated and stored in the object
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)){        ## return the cached inverse if it exists
        return(i)
    }
    data <- x$get()         ## get the matrix data
    i <- solve(data, ...)   ## calculate the inverse
    x$setinv(i)             ## store the inverse in the object 
    i                       ## Return the inverse of 'x'
}
