## Two functions that work together to calculate the invers and cache it 
## First function makes a list of functions to be used, they are the 
## setting and geting of values of matrix x, and the same for the inverse

## Makes a list of functions for use in the function cache solve

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        # when matrix x changes its value to y, the inverse is reset
        set <- function(y){
                x <<-y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculaiton of matrix x inverse or returning of cached inverse if exists

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
