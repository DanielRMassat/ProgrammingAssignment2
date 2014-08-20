## Put comments here that give an overall description of what your
## functions do
# Both function work behave individually and collectively.
## Write a short comment describing this function
# This function stores in his enviroment a matrix and his inverse if it exists
makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        #The set() function erases any existing computed matrix inverse
        set <- function(z){
                x <<- z
                I <<- NULL
        }
        get <- function() x
        setinv <- function(inv) I <<- inv
        getinv <- function() I
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# Checks whether the computed inverse exists and reacts accordingly
# simply return the existing inverse matrix or calculates the inverse of the current matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return (I)
        }
        matriX <- x$get() #obtain the matrix from the cache created by makeCacheMatrix()
        I <- solve(matriX, ...)
        x$setinv(I)
        I
}

