## makeCacheMatrix is the constructor function. cacheSolve calculate the invert of the matrix and replaces it with null everytime the 
## matrix changes (until the cache is being recalculated)

## makeCacheMatrix the a matrix as parameter. "m" is set to null as default everytime a new matrix is inputed. The "set" subfunction again allocates null to "m" 
## since there was a change in the initial matrix and now the cache needs to be recalculated. Get return the matrix input. setinverse  the inverted value, this value is taken from "m" which is the variable where the 
## cacheSolve calculated and stores the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
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


## this function checks first that the matrix cache was calculated or not. If it was calcualted (aka not NULL) then it returns the value. Otherwise it calcualtes it using the solve function and it stores the result into "m". 


cacheSolve <- function(x, ...) {
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
