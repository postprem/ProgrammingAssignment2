## Last Update: July 26, 2014; Author: Premnath Padmanabhan
## 
## This R file contains two functions - 1. makeCacheMatrix; 2. cacheSolve
## 1. makeCacheMatrix - provides a list of functions to set and get matrix and it's inverse
## 2. cacheSolve - given a list of function it computes and returns the inverse of matrix

## makeCacheMatrix Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## initialize inv, variable to store inverse matrix
    inv <- NULL
    
    ## Setter for matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Getter for matrix
    get <- function() x
    
    ## Setter for inverse
    setinv <- function(inverse) inv <<- inverse
    ## Getter for inverse
    getinv <- function() inv
    
    ## Return the matrix with our newly defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve (x, ...)
## input x = list of functions to get, set matrix and get, set inverse matrix; 
## can take additional arguments/parameters
## checks to see if the cache has inverse matrix, if not, it computes, saves and returns

cacheSolve <- function(x, ...) {
    
    ## Get inverse
    inv <- x$getinv()
    
    ## Does the inverse exits, if so return matrix
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If not, calculate now
    data <- x$get()
    ## using solve to inverse the matrix
    inv <- solve(data, ...)
    
    ## Set in Cache to reduce computation in future
    x$setinv(inv)
    
    ## Return the matrix
    inv
    
}