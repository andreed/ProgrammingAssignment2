## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# first line like in forked file: Function makeCacheMatrix expects   
# a matrix (can't create a new)
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL            ## s will hold the inversed matrix later
    set <- function(y) {  ## function to initialize variables in parent env
        x <<- y
        s <<- NULL
    }
    get <- function() x          ## return the matrix
    setsolve <- function(solve) s <<- solve   ## set the inverse matrix
    getsolve <- function() s                ## get the inverse matrix (from)
    list(set = set, get = get,      ##  by this, an object made by makeCacheMatrix
         setsolve = setsolve,       ##  ..can use myMatrix$setsolve and the like to
         getsolve = getsolve)       ##  ..access the functions defined above
}


## Write a short comment describing this function

## only works with objects, that have been created through makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()       ## call setsolve-function to look for cached data
    if(!is.null(s)) {       ## cached data found, so return that
        message("getting cached data")
        return(s)
    }
    data <- x$get()         ## no cached data found, so get the matrix
    s <- solve(data, ...)   ## calculate the inverse matrix
    x$setsolve(s)           ## store that inversed matrix in the cache
    s                       ## return the inversed matrix to the user
}
