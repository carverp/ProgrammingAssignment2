## cachematrix.R
##
## Description: 
##  Since Matrix inversion is usually time-consuming computation, 
##  these functions will work together to provide a caching mechanism 
##  for storing the inverse of a matrix.
##
##  Caching will allow for a calculating the inverse only when needed 
## (if no value is cached and if the matrix has not changed).
##
##  makeCacheMatrix(x):
##      - will create an object that will store a Matrix and its Inverse
##      
##  cacheSolve(x, ...) :
##      - will return the cached/calculated Inverse of a given Matrix
##
## Example:
##  myMatrix <- matrix(c(1,1,1,3,4,3,3,3,4), nrow=3, ncol=3)
##  cachedMatrix <- makeCacheMatrix(myMatrix)
##  cacheSolve(cachedMatrix)
## 
##---------------------------------------------------------------------------


## Creates a list that contains the functions for:
##  - storing and retrieving the Matrix
##  - storing and retrieving the Inverse of the Matrix
##
## Arguments: 
##  x   Matrix for which Inverse is to be calculated
##
## Value returned: 
##  List of functions used to store/access Matrix and Inverse

makeCacheMatrix <- function(x = matrix()) {

    ## Initialize the Inverse of Matrix - Cache
    inverseMatrix <- NULL
    
    ## Function to Set the value of the Matrix
    set <- function(val){
        ## set new value for the Matrix
        x <<- val
        
        ## Clear the Inverse since Matrix has changed
        inverseMatrix <- NULL
    }
    
    ## Function to Return the value of the Matrix
    get <- function() x
    
    ## Function to Sets the value of the Inverse
    setInverse <- function(inv){
        inverseMatrix <<- inv
    }
    
    ## Function to Get the value of the Inverse
    getInverse <- function() inverseMatrix
    
    ## Return the list of functions
    list(set = set, get = get, setInverse=setInverse, getInverse=getInverse)
}


## Calculates and returns the Inverse of the Matrix stored using 
## makeCacheMatrix. If the Matrix already has a cached Inverse, 
## the calculation is skipped and the cached value is returned.
##
## Arguments: 
##  x   list returned by makeCacheMatrix.
##  ... further arguments passed to 'solve'
##
## Value returned: 
##  Matrix  the inverse of the specified Matrix

cacheSolve <- function(x, ...) {
    
    ## Get Inverse from Cache
    inverseMatrix <- x$getInverse()
    
    
    if(!is.null(inverseMatrix)){
        ## If Inverse IS Cached
        message("Retrieving from cache")
        return(inverseMatrix)
    }
    
    ## Inverse needs to be calculated ##
    data <- x$get() ## Get Matrix that was stored
    
    inverseMatrix <- solve(data,...) ## Calculate Inverse
    
    x$setInverse(inverseMatrix) ## Store Inverse to Cache
    
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix
}
