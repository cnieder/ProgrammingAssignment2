## The first function makeCacheMatrix creates a matrix that can cache its inverse.
## The list at the end of the function
## conatins all the variables that are returned by the function.
## These variables can then be called by the second function cachesolve.  
## Cachesolve returns a matrix that is the inverse of the matrix x.
## If the inverse of x has already been calculated and the matrix has not changed, 
## the inverse should be retrieved from the cache.


## Set, a function within makeCacheMatrix, takes "y" as an argument. 
## Y is assigned to x, the matrix-argument in 'makeCacheMatrix'. 
## When calling a$get() and a$getinv the function returns matrix x and Null  
## for the inverse of x, if no value for the inverse of x had been previously stored in the cache. 
## x and inv = Null are available outside this function because of the supra assign-opperator. 

        makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL 
        set <- function(y) {
                x <<- y
                inv <<- NULL          
        }
        
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cachesolve takes one argument, e.g. 'a', which is linked to the first function
##'makeCacheMatrix' and 'x' through the expression a <- makeCacheMatrix(x).
## If the inverse of x has already been calculated and the matrix has not changed, 
## the inverse should be retrieved from the cache (a$getinv()).
## This is indicated by the "getting cached data"-statement. 
## If the matrix changed and there is no cached value (a$getinv() = NULL),
## the inverse of the new matrix will be computed. 

                cachesolve <- function(x, ...) {
                
                inv <- x$getinv()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                
                data <- x$get()
                inv <- solve(data, ...)  
                x$setinv(inv) 
                inv 
        }