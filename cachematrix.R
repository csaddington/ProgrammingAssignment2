## R PROGRAMMING - ASSIGNMENT 2

## Rather than computing the inverse of a matrix repeatedly and slowing down 
## performance, we want to, first, write a function that creates a special 
## "matrix" object that can cache its inverse, and then, second, write a 
## function that can compute the inverse of this special "matrix", or use the
## previously cached inversion assignment.

## PART 1

## The required makeCacheMatrix function will create a list containing the 
## individual functions required to:

## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of inverse of the matrix
## 4. Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { ## Changes stored matrix in main function.
        x <<- y          ## Subs x with y (the input) in the main function.
        inv <<- NULL     ## Restores null value, as old matrix no longer
                         ## required when new matrix passed as argument.
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse ## Assigns 'inv' in deeper
                                                    ## environment to be cached
    
    getinverse <- function() inv  ## Return list containing four functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## PART 2

## The second function in the set, the 'cacheSolve' function, returns the 
## inverse of the matrix. It begins by checking if the inverse has been cached 
## by the 'makeCacheMatrix' function. If no cached matrix inversion is present,
## the cacheSolve function computes the inverse, and also sets the value in the 
## cache using the 'setinverse' function.

## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse() ## Accesses original function call and any associated
                          ## data in environment that the function references.

    if(!is.null(inv)) {  ## If the assigned inverse is not null, return
                         ## retrieved matrix inverse.
        message("Solved using retrieved cached data!")
        return(inv)
    }
    data <- x$get()      ## If the assinged inverse is null, start process of  
    inv <- solve(data)   ## inverting matrix 'x' by calling object x$get().
    x$setinverse(inv)    ## Assigns the matrix inversion to the 'setinverse' 
                         ## function.
    message("Solved with no cached data!")
    return(inv)
}