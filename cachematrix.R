##  Write the following functions:

##  makeCacheMatrix: This function creates a special "matrix" object that can
##  cache its inverse.

##  cacheSolve: This function computes the inverse of the special "matrix"
##  returned by makeCacheMatrix above. If the inverse has already been calculated
##  (and the matrix has not changed), then cacheSolve should retrieve the inverse
##  from the cache.

## Put comments that give an overall description of what your functions do


## Write a short comment describing this function:
## makeCacheMatrix creates a place for us to cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix())  {
    
    ##  first create a place to put the inverted matrix data
    inverted <- NULL
    
    ##  next we change the vector
    set <- function(y) {
        
        ##  and substitute the vector x with y (the input) in the main function
        ##  (makeCacheMatrix)
        x <<- y
        
        ##  restore the value of 'inverted' to null
        inverted <<- NULL
    }
    
    ##  get the values of the matrix
    get <- function() x
    
    ##  store the value of the input in variable 'inverted' into the main
    ##  function makeCacheMatrix
    setinverse <- function(inverse) inverted <<- inverse
    
    ##  return the value stored immediately above
    getinverse <- function() inverted
    
    ##  To store the 4 functions in the function makeCacheMatrix, we need
    ##  the function list(), so that when we assign makeCacheMatrix to an object,
    ##  the object has all the 4 functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##  Write a short comment describing this function:
##  The following function calculates the inverse of the special "matrix" created
##  with the above function. However, it first checks to see if the inverse has
##  already been calculated. If so, it retrieves the inverse from the cache and
##  skips the computation. Otherwise, it calculates the inverse of the data and
##  sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    
    ##  GOAL: Return a matrix that is the inverse of 'x'
    
    ##  first, cacheSolve needs to verify the value 'inverted' (stored previously
    ##  with getinverse) exists and is not NULL. 
    ##  If it exists in memory, it returns a message and the value 'inverted',
    ##  that is supposed to be the invrse, but not necessarily.
    
    inverted <- x$getinverse()
        if (!is.null(inverted)) {
            message("retrieving cached data")
            return(inverted)
        }
    
    ##  everything that follows is basically an "else { }"
    
    ##  'newmatrix' gets the matrix stored with makeCacheMatrix
    newmatrix <- x$get()
    
    ##  The solve function computes the inverse of a square matrix.
    ##  Here, solve() returns the inverse of 'newmatrix' into 'inverted'
    inverted <- solve(newmatrix, ...)
    
    ##  stores the inverse in the object generated with makeCacheMatrix
    x$setinverse(inverted)
    inverted
}