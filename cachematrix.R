## Used together these two functions create a cache for a matrix's inverse.

## The first function creates a list for your inputed function. This list
## is basically a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This second function takes the output of the above function and computes
## your original matrix's inverse.  The function checks to see if the inverse
## has already been calculated. If so it returns the inverse from the cache
## and skips any further computation. Otherwise it calculates the inverse of
## the matrix and sets the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
