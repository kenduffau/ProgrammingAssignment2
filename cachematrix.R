## These functions stores contents and the inverse of a given matrix (cached in makeCacheMatrix after
## invoking the function) and then check the cached contents when cacheSolve is invoked. 

## Stores contents, invokes and caches inverse of given matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Checks contents of cache and if present, uses those values. If not, it generates the inverse

cacheSolve <- function(x = matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
