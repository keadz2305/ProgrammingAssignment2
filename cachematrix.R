## Yee Keat Leong
## R-programming Wk3 Asgn2

## Below are two functions that are used to create a special object 
## that stores a matrix and cache the inversed matrix.

## makeCahceMatrix creates a special "vector"
## which is really a list contain a list of functions to:
## 1) sets the matrix
## 2) gets the matrix
## 3) sets the inverse of the marix
## 4) gets the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
        im <- NULL
        set <- function(y) {
                m <<- y
                im <<- NULL
        }
        get <- function() m
        setSolve <- function(solve) im <<- solve
        getSolve <- function() im
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
        
}


## Inverses the matrix of vector the created with the above function. 
## It first checks to see if the matrix has already been inversed. 
## If so, it gets the inversed matrix from the cache and skips the computation. 
## Otherwise, it inverses the matrix 
## and sets the matrix in the cache via the setSolve function

cacheSolve <- function(m, ...) {

        
        im <- m$getSolve()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        mx <- m$get()
        im <- solve(mx, ...)
        m$setSolve(im)
        im
}
