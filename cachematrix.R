## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
       cache <- NULL
       set <- function(y) 
		{
                x <<- y
                cache <<- NULL
        	}

        get <- function() x

        cacheInv <- function(solve) cache <<- solve

        getInv <- function() cache
        
        list(set = set, get = get, cacheInv = cacheInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inv <- x$getInv()
        if(!is.null(inv)) 
	{
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$cacheInv(inv)
        inv
}
