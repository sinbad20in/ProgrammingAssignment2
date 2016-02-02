## This function is going to return a list which inturn is function which is going to
## assign values to the matrix. More comments inline


makeCacheMatrix <- function(x = matrix()) {
    matrixInv <- NULL
    set <- function(y){
        x <<- y
        matrixInv <<- NULL
    }
    # Set functions set the value of argument passed
    get <- function() x
    # Get returns the argument passed - used for fetching
    setMatrixInv <- function(invMatrix) matrixInv <<- invMatrix
    # setMatrixInv will assign the argument passed to matrixInv variable
    getMatrixInv <- function() matrixInv
    # getMatrixInv will return the value of matrixInv
    list( set = set, get = get,
          setMatrixInv = setMatrixInv,
          getMatrixInv = getMatrixInv)
}

## This function checks if the value is already cached, is so returns the cached
## value, else it computes the inverse of matrix

cacheSolve <- function(x, ...) {
    matrixInv <- x$getMatrixInv()
    if(!is.null(matrixInv)){
        message("getting cached matrix data")
        return(matrixInv)
    }
    data <- x$get()
    matrixInv <- solve(data, ...) #computes inverse if not found in cache
    x$setMatrixInv(matrixInv)
    matrixInv
}

