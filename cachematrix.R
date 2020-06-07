## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its reverse.


makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        set <- function(y){
                x <<- y
                inverted <<- NULL
        }
        
        get <- function()x
        setInverse <- function(inverse) inverted <<- inverse
        getInverse <- function() inverted 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted <- x$getInverse()
        if(!is.null(inverted)){
                message("getting cached data")
                return(inverted)
        }
        mat <- x$get()
        inverted <- solve(mat,...)
        x$setInverse(inverted)
        inverted
        
}


