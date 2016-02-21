## R Programming Assignment 2 Functions
## Lexical Scoping - Caching the Inverse of a Matrix
## Two functions to cache the inverse of a Matrix

## This assignment assumes that matrix supplied is always invertable

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## set the value of the matrix
        set <- function(y) {
                ## <<- assign object to different environment
                x <<- y
                inv <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the matrix
        setSolve <- function(solve) inv <<- inv
        
        ## get the value of the matrix
        getSolve <- function() inv
        
        list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)
}


## Computes inverse of the special "matrix" returned by makeCacheMatrix
## If inverse has already been calculated and matrix has not changed
## cacheSolve should retreive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## call getSolve and assign to inv
        inv <- x$getSolve()
        
        ## if inv is null, get and return cached data
        if(!isnull(inv)){
                message("Getting cached data")
                return(inv)
        }
        
        ## if it is not null, get and set
        
        data <- x$get()
        inv <- solve(data)
        x$setSolve(inv)
        inv
        
}
