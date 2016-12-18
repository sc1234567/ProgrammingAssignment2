## Caching the inverse of a matrix

rm(list=ls())
setwd('~/Desktop/Coursera/R/Week_3')

## Load MASS library to use ginv command as solve did not work
library(MASS)

## Creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {             # initialize x and m
        m <- NULL
        set <- function(y) {                            
                  x <<- y                               # assigns input argmuent to x in parent environment
                  m <<- NULL                            # assigns value of NULL to m in parent environment
        }
        get <- function() x                             # retrieves x from parent environment
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}


## Computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calulated and the matrix has not changed,
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- ginv(data)
        x$setinv(m)
        m
}

x <- matrix(1:16, 4, 4)
a <- makeCacheMatrix(x)
