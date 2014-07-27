## This function calculate the inverse of a matrix.
## If the inverse of the matrix has been calculated, it will be cached

## Caching the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## check if the inverse of a matrix has been calculated. If yes,display it
## or calculate and cache it.

cacheSolve <- function(x, ...)
{
    m <- x$getinverse()
    if(!is.null(m)) 
    {
        message("getting cached inverse of matrix")
        return(m)
    }
    matri <- x$get()
    m <- solve(matri) #calcutae the inverse of 'X'
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
