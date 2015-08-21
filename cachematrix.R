## These two functions provide a way to set a matrix and calculate its inverse. 
## The inverse can be cached away and called upon without recalculating. 

## Creates a list that holds four functions able to set/get the value of a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {                                           # reset value of matrix and inverse
                x <<- y
                inv <<- NULL
        }
        get <- function() x                                            # return x
        setInv <- function(inverse) inv <<- inverse                    # set inv to inverse
        getInv <- function() inv                                       # return inverse value
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Checks to see if matrix inverse has been calculated. If not it calculates it and stores it to inv.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()                                              # get inv value
        if(!is.null(inv)) {                                            # if exists, return the value
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                                                # else get matrix, compute inverse, set inverse
        inv <- solve(data,...)
        x$setInv(inv)
}
