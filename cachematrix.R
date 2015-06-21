## Function is to cache the inverse of an inversible matrix

## The function makeCacheMatrix creates a list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix())
{
    ## 1. Initialising variable 'invmat' to NULL
    invmat <- NULL
    
    ## 1. Declaring the 'set' function to store the input matrix and
    ## initialise the inverse matrix
    ## 2. Both these variables are outside the current environment
    set <- function(y) 
    {
        x <<- y
        invmat <<- NULL
    }
    
    ## 1. The function get() returns the stored input matrix
    get <- function()
        x
    
    ## 1. Stores the computed matrix inverse to the variable invmat
    setinvmat <- function(inv)
        invmat <<- inv
    
    ## 1. Function to return the inverse matrix
    getinvmat <- function() 
        invmat
    
    ## 1. Returns the list to the function makeCacheMatrix
    list(set = set, 
         get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
}


## The function cacheSolve checks to see if inverse of the matrix has already computed.
## If yes, it returns the cached value, else performs the computation. 

cacheSolve <- function(x)
{
    ## 1. Checks if inverse matric already present in cache
    ## 2. If present, returns the inverse matrix
    inv <- x$getinvmat()
    if(!is.null(inv)) 
    {
        message("getting cached data")
        return(inv)
    }
    
    ## 1. Stores the input matrix to the variable 'data'
    data <- x$get()
    
    ## 1. Computes the inverse of the matrix and stores in the variable 'inv'
    inv <- solve(data)
    
    ## 1. Caches the computed matrix invetse in cache using the function 'setinvmat' 
    x$setinvmat(inv)
    
    ## 1. Returns the matrix inverse to the function 'cacheSolve'
    inv
}