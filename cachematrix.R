## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL # sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
    y <- NULL 
    setmatrix <- function(y) { #set the value of the matrix
        x <<- y ## caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
        m <<- NULL # # sets the value of m (the matrix inverse if used cacheSolve) to NULL
    }
    
    list(setmatrix = setmatrix, getmatrix = getmatrix, # creates a list to house the four functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getInv() # get the inversed matrix from object x
    # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
    if(!is.null(m)) { # if the inversion result is there
        message("getting cached data")
        return(m) # return the calculated inversion
    }
    data <- x$get() # if not, we do x$get to get the matrix object
    m <- solve(data) # we solve it
    x$setInv(m) # we then set it to the object
    m # return the solved result
}