## This pair of functions caches the inverse of a matrix. But first, it check if the matrix is the same as before and if the inverse has already been cached. If there's already an inverse cached, it retrieves the inverse, otherwise it sets a new inverse and cahces it.
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # sets the value of m - the inverse of the matrix - to NULL
    y <- NULL # sets the value of y to NULL
    setmatrix <- function(y) { #set the value of the matrix
        x <<- y ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
        m <<- NULL # # sets the value of m (the matrix inverse if used cacheSolve) to NULL
    }
    getmatrix <- function() x
    setmatrix <- function(inverse) m <<- inverse
    getmatrix <- function() m
    list(setmatrix = setmatrix, getmatrix = getmatrix, # creates a list to house the four functions
   setinverse = setinverse,
   getinverse = getinverse)
}


## The function below, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
       
    # First, check that the inverse hasn't already been cached
m <- x$getinverse() # if an inverse has already been calculated this gets it
if(!is.null(m)){ # check to see if cacheSolve has been run before
    if(x$setmatrix() == x$getmatrix()) { # check that matrix is the same, then send a text message and return the cached matrix inverse
        message("getting cached data")
        return(m)
    }
# otherwise 
data <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
x$setmatrix(data) # run the setmatrix function on the input matrix to cache it
m <- solve(data, ...) # compute the value of the inverse of the input matrix
x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
m # return the inverse
}
