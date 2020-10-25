## makeCacheMatrix creates 4 functions that can be called by cacheSolve to solve
## and store the values of a matrix

## cacheSolve uses these functions to
## a) return the solution if it has already been calculated OR
## b) calculate the solution and store it

## This function returns a list with 4 functions: set, get, getsolution, and setsolution
## set: caches the value of the matrix
## get: returns the value of the matrix as stored by set function
## setsolution: calculates the inverse of the matrix and caches it
## getsolution: returns the inverse of the matrix as stored by setsolution function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolution <- function(solve) m <<- solve
    getsolution <- function() m
    list(set = set, get = get, set_solution = setsolution, get_solution = getsolution)
}


## This calls the get_solution element of x, which is the getsolution function.
## IF the value is not null, then it prints a message and returns the cached value.

## ELSE, it calls the get element of x, which is the get function,
## calculates the inverse of that matrix, caches that value using setsolution function,
## and then returns the inverse.

cacheSolve <- function(x, ...) {
    m <- x$get_solution()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_solution(m)
    m
}
