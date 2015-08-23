## makeCacheMatrix and cacheSolve in combimation, 
## Calculate the inverse of a given matrix and store
## it in cache to avoid the calculation every time the
## fuctions are called for the same given matrix.


## makeCacheMatrix is a function that will cache
## a given matrix (argument of the function=matriz) and its
## inverse. Everytime the function is called or a new
## matrix is set, the variable inv_matriz is set to NULL.
## The function returns a list of 4 fuctions:
## 1) to set the new matrix (matriz$setM()) 3) its inverse
## 2) to get the matrix (matriz$getM())
## 3) to set inverse (matriz$setInvM())
## 4) to get inverse (matriz$getInvM())

## This function do not calculate the inverse of the matrix.

makeCacheMatrix <- function(matriz = matrix()) {

	  inv_matriz <- NULL

	  set <- function(y) {
                matriz <<- y
                inv_matriz <<- NULL
        }

        get <- function() matriz

        set_inv_matriz <- function(InvMatriz) inv_matriz <<- InvMatriz

        get_inv_matriz <- function() inv_matriz

        list(setM = set, getM = get,
             setInvM = set_inv_matriz,
             getInvM = get_inv_matriz)
}

## cacheSolve calculates the inverse of the matrix set
## by the function setInvM() in makeCacheMatrix().
## if there is a value stored in getInvM(), then the
## the calculation is skipped and the value is retrived.
## if calling getInvM() returns a NULL value, then the
## Inverse is calculated.

cacheSolve <- function(x, ...) {
        InvMatriz <- x$getInvM() 
        if(!is.null(InvMatriz)) {		##if m  not NULL then will enter the IF loop
                message("getting cached data")
                return(InvMatriz)
        }
        data <- x$getM()
        InvMatriz <- solve(data, ...)
        x$setInvM(InvMatriz)
        InvMatriz
}