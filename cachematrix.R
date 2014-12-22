## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    cache <- NULL
    
    get <- function() x
    set <- function(y) {
        
        if(!all(x==y)){
            x <<- y
            cache <<- NULL
        }
    } 
    getinv <- function() cache
    setinv <- function(inv) cache <<- inv
    
    list ( set = set, get = get,
           setinv = setinv,
           getinv = getinv)
    
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    cache <- x$getinv()
    
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }
    
    data <- x$get()
    cache <- solve(data)
    x$setinv(cache)
    cache
}
