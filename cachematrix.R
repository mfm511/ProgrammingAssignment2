## Creates a cache version of Solve() function in R. 
## 

## This function creates a cached matrix object of the list type with four
## function elements. Each of the four functions can be use to get and set
## the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function takes an object returned by the makeCacheMatrix() function
## as its argument and return the inverse of the matrix defined in the
## object. It first search if the inverse is already set and stored in one
## of the variables. If it is already set it just return the value and if not
## it will compute the inverse and store it in a variable.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
