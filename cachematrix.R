## This scripts provides two functions that can be used for caching inverse of
## of a matrix after first time computation.

## Use this matrix if you want to cache the inverse of it later. Pass your matrix
## to this function and use the returned list to access the cached inverse (or the
## matrix itself). This fuction accepts a matrix and has another variable to store 
## the inverse of the given matrix. It returns a list of four functions: 1. setting
## the matrix, 2. setting and storing the inverse of the matrix (when computed - is 
## NULL by default), 3. getting the matrix that was stored earlier and 4. returning
## the stored inverse matrix (or NULL if is not computed yet)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solved_inv) inv <<- solved_inv
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Use this function to get the inverse of a matrix that you have passed to makeCacheMatrix 
## before. After the first call, it computes the inverse and sends the result to the 
## makeCacheMatrix environment to be stored there. After the first call, it calls an internal 
## function from makeCacheMatrix to return the already computed and cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("returning cached inverse")
                return (inv)
        }
        inv <- solve(x$get())
        x$setInverse(inv)
        inv
}
