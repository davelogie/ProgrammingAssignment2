## Put comments here that give an overall description of what your
## functions do

## Functions for programming assignment 2 which 
##    a) Cache the Mean of a Vector
##    b) Cache the Inverse of a Matrix

## Write a short comment describing this function

## ** Cache the Mean of a Vector

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean(mean)
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


## Write a short comment describing this function

## ** Cache the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
        v <<- NULL
        x
}

## ** Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        if(!is.null(v)) {
                message("getting cached data")
                return(v)
        }
        toinvert <- makeCacheMatrix(x)
        v <<- solve(toinvert)
        v
}
