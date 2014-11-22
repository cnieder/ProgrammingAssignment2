makeCacheMatrix <- function(x = matrix()) {
        ##  The function creates a matrix that can cache its inverse.
        
        inv <- NULL  ## NULL is assigned to the inv-variable 
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
               
                ## set is the name of a function within makeCacheMatrix.
                ## Set takes "y" as an argument. Y is assigned to x, which is 
                ## the matrix-argument for 'makeCacheMatrix'. Through the 
                ## The function returns x and inv = Null  when calling a$get() and a$getinv if 
                ## no value is has been stored in the cache yet. 
                ## These variables are available outside this function because of the supra assign-opperator.  
        }
        
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##The list conatins all the variables that are be returned by the function.
## These variables can now be called by the second function cachesolve.  

        
        cachesolve <- function(x, ...) {
                ##cachesolve takes one argument, e.g. 'a', which is linked to the first function
                ##'makeCacheMatrix' and 'x' through a <- makeCacheMatrix(x).
                ##Cachesolve returns a matrix that is the inverse of 'x' .
                ##If the inverse has already been calculated and the matrix has not changed, 
                ##the inverse should be retrieved from the cache (a$getinv()).
                ## This is indicated by the "getting cached data"-statement. 
                
                inv <- x$getinv()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                
                ## If the matrix changed and there is no cached value (a$getinv() = NULL),
                ## the inverse of the new matrix will be computed. 
                data <- x$get()
                inv <- solve(data, ...)  ## computates the inverse of matrix x
                x$setinv(inv) ## sets a new inverse in the cache
                inv 
        }