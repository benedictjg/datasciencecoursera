##The following pair of functions can compute the inverse of a matrix
##and cache the solution to be used later instead of re-computing.

##The makeCacheMatrix function creates a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse of the matrix
##4. get the value of the inverse of the matrix

makeCacheMatrix <- function(mx = matrix()){
        mxinv <- NULL
        set <- function(x) {
                mx <<- x
                mxinv <<- NULL
        }
        get <- function() return(mx)
        setinv <- function(inv) mxinv <<- inv
        getinv <- function() return(mxinv)
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##The cacheSolve function calculates the inverse of the "matrix" created
##with the makeCacheMatrix function.  However, it first checks to see if
##the inverse has already been calculated.  If so, it gets the inverse from
##the cache and skips the computation.  Otherwise, it calculates the inverse of
##the matrix and sets the value of the inverse in the cache via the setinv
##function.

cacheSolve <- function(mx, ...) {
        mxinv <- mx$getinv()
        if(!is.null(mxinv)) {
                message("Getting cached data...")
                return(mxinv)
        }
        data <- mx$get()
        mxinv <- solve(data, ...)
        mx$setinv(mxinv)
        return(mxinv)
}