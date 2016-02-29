## Usage : 1. m <- makeCacheMatrix(matrix) to create a cache matrix
##         2. cacheSolve (m)  # to set and get cached inverse matrix 

## makeCacheMatrix function creates a SPECIAL "matrix" object 
## that can cache its inverse
## Note: for now assuming that x is invertible, no error handling provided
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL;
    set <- function(y) {
        x <<- y;
        m <<- NULL;
    }
    get <- function() x;
    setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix;
    getInverseMatrix <- function() m;
    list(set = set, get = get,
        setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## cacheSolve function computes the inverse of the special "matrix" 
## created by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then retrieve the inverse from the cache
cacheSolve <- function(x) {
    m <- x$getInverseMatrix();
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get();
    m <- solve(data);
    x$setInverseMatrix(m);
    m;
}