## This function creates a special "matrix", which is really a list containing a function to
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse of the matrix
## 4- get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix())
{
    ## this is the cached inverse
    inv <- NULL
    
    ## this sets the matrix and clears the cached inverse
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    ## this gets the matrix
    get <- function() x
    
    ## this caches the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## this gets the cached inverser
    getinverse <- function() inv
    
    ## this creates a list of the just created functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with
## the above function. However, it first checks to see if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function.
cacheSolve <- function(x, ...)
{
    ## get the cached inverser
    inv <- x$getinverse()
    ## if there is a value, return it
    if(!is.null(inv))
    {
        message("getting cached inverse")
        return(inv)
    }
    
    ## if there isn't a cached inverse
    ## get the matrix
    data <- x$get()
    ## calculate the inverse
    inv <- solve(data, ...)
    ## cache it
    x$setinverse(inv)
    inv
}
