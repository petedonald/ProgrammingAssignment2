##  makeCacheMatrix and cacheSolve
##  a set of functions to cache the inverse of a matrix


## makeCacheMatrix
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    
    i <- NULL
    
    # set the matrix
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }

    # get the matrix
    get <- function(){
        m
    }
    
    # set the inverse of the matrix
    setInv <- function(inverse) {
        i <<- inverse
    }
    
    # get the inverse of the matrix
    getInv  <- function() {
        i
    }
    
    # list methods
    list ( 
        set = set, 
        get = get, 
        setInv = setInv,
        getInv = getInv)    
}


## cacheSolve
## computes the inverse of the special "matrix" returned by makeCacheMatrix
## Retrieves inverse from cache if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    
    
    #if inverse is already set, return value from cache
    if(is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    #get matrix from object
    data <- x$get()
    
    # compute inverse square of matrix 
    m <- solve(data) %*% data
    
    x$setInv(m)
    
    return(m)
    
}
