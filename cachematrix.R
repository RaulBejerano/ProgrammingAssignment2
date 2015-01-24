## Cache the inverse of a matrix


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function (y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inver) inverse <<- inver
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculates the inverse of a matrix
## If the inverse had been saved in the cache it doesn`t need to calculate again

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
