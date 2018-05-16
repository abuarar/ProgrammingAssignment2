## makeCacheMatrix function creates a list of functions that do the following: 
## -set the value of a matrix : setMatrix
## -get the value of a matrix : getMatrix
## -set the value of the inverse of matrix : setMatrixInverse
## -get the value of the inverse of matrix : getMatrixInverse
## cacheSolve function will save us the time of computing the inverse if it is already cached in makeCacheMatrix by giving the cashed value of inverse of the matrix saying: "getting cached matrix inverse" , but in case the cashed value is NULL, then it will calculate the inverse of the matrix that is set in makeCacheMatrix, 

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        setMatrix <- function(y) {
                x <<- y
                I <<- NULL
        }
        getMatrix <- function() x
        setMatrixInverse <- function(Inverse) I <<- Inverse
        getMatrixInverse <- function() I
        list(setMatrix = setMatrix , getMatrix = getMatrix ,
             setMatrixInverse = setMatrixInverse ,
             getMatrixInverse = getMatrixInverse )
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        I <- x$getMatrixInverse ()
        if(!is.null(I)) {
                message("getting cached matrix inverse")
                return(I)
        }
        data <- x$getMatrix()
        I <- solve(data)
        x$setMatrixInverse(I)
        I
        ## Return a matrix that is the inverse of 'x'
}
