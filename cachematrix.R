## This pair of functions caches the inverse of a matrix to avoid compiting it repeatedly

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
        setMat <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        getMat <- function() x
        setInv <- function(inverse) invMat <<- inverse
        getInv <- function() invMat
        list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
        

}


## This function computes the inverse of the matrix object from the above function or retreives the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInv()
        if(!is.null(invMat)) {
                message("getting cached inverse")
                return(invMat)
        }
        data <- x$getMat()
        invMat <- solve(data, ...)
        x$setInv(invMat)
        invMat
}
