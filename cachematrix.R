# cacheMatrix.R provides two functions that can be used to optimize application
# of the solve() function on a matrix:
# makeCacheMatrix() is called passing the source matrix as an argument
# cacheSolve() is then called passing the result of makeCacheMatrix().
# On the first call to cacheSolve, the inverse is calculated and returned.
# On subsequent calls, the cached inverse is returned.

# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# setInverse sets the inverse of the matrix
# getInverse gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# The second function calculates the inverse of the special "matrix"
# created by makeCacheMatrix(). However, it first checks to see if the
# inverse has already been calculated by calling the getInverse function.
# If the inverse exists, it is returned and skips the computation. 
# Otherwise, it calculates the inverse of the data using solve() and 
# sets the inverse value of the matrix in the cache via the `setInverse` function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) 
    x$setInverse(i)
    i
}