##The first function, makeCacheMatrix calculate the inverse of a matrix and saves it
## to the cache: 
    ##set the value of the matrix
    ##get the value of the matrix
    ##set the value of the inverse
    ##get the value of the inverse
## makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
       ## no value is cached so set cache to NULL in the beginning
       cache <- NULL
       ## store a matrix
       set <- function(y) 
		{
                ## assign the input matrix y to the variable x
                x <<- y
                ## because the matrix is assigned a new value, re-initialize 
                ## the cache to NULL
                cache <<- NULL
        	}
	## get the value of a matrix
        get <- function() x
	## cache the given argument
        cacheInv <- function(solve) cache <<- solve
	## get the cached value inverse of x
        getInv <- function() cache
        ## return a list
        list(set = set, get = get, cacheInv = cacheInv, getInv = getInv)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'cacheInv' function
cacheSolve <- function(x, ...) {
                ## get the cached value'
                inv <- x$getInv()
        ## if a cached value exists return it
        if(!is.null(inv)) 
	{
                message("getting cached data")
                return(inv)
        }
        ## caclulate the inverse and store it in the cache
        data <- x$get()
        inv <- solve(data, ...)
        x$cacheInv(inv)
        ## Return a matrix that is the inverse of 'x
        inv
}
