## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly. It is done by imitating the example of
## caching the mean of a vector.

## makeCacheMatrix This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
          x <<- y
          invm <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invm <<- inverse
        getinverse <- function() invm
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix", if
## the inverse was not calculated before, returned by
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinverse()
        if(!is.null(invm)) {
          message("getting cached data.")
          return(invm)
        }
        data <- x$get()
        invm <- solve(data)
        x$setinverse(invm)
        invm
}
