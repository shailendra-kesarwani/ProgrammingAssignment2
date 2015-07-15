## Following pair of functions devise a way to ##
## cache the inverse of a matrix               ##
## as calculating inverse of a matrix is time  ##
## consuming and costly computation            ##

#This function creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL            # i stands for inverse
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


#This function computes the inverse of the special matrix 
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        print("Getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)  # ... are function arguments that can be passed to solve
    x$setInverse(i)
    i # Return a matrix that is the inverse of 'x'
}