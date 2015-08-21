## This function creates a special "matrix" object that can cache its inverse
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) 
{
    ##initialization of inverse
    inv <-NULL
    
    ##Function to set
    set <- function( mtx ) 
    {
      m <<- mtx
      inv <<- NULL
    }
    ##Function to get
    get <- function() 
    {
      m
    }
    ##Function to set inverse
    setInverse <- function(inverse) 
    {
      inv <<- inverse
    }
    ##Function to get inverse
    getInverse <- function() 
    {
      inv
    }
    ##Function to return a list 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## CheckiIf the inverse has already been calculated (and the matrix has not changed). 
        ## If so return inverse from the cache
        if( !is.null(m) ) 
        {
          message("getting cached data")
          return(m)
        }
        
        ##get the matrix from input object passed
        data<-x$get()
        
        ##Compute inverse of the matrix using solve function in R
        m<-solve(data)
        
        ##set inverse to the object
        x$setInverse(m)
        
        ##Return the inverse
        m
}
