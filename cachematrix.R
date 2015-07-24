## The function makeCacheMatrix, returns a special matrix which 
## is capable of caching inverse of a matrix. The function contains
## two variable x for storing the matrix, and i for storing the 
## inverse of the matrix. These variables are local to the function
## and not defined in the .GlobalEnv

## The special matrix returned contains four functions set, get, 
## setinv, and getinv. The variables x, and i accessed as  free 
## variables in these four functions. However to update the free
## variable the operator <<- is used.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
	# This function "set", initializes the free varible x  which is  
	# matrix. It also initializes, free variable i which is inverse of x.
	set <- function(y) {
         x <<- y
         i <<- NULL
    }
	# The function "get", returns matrix . 
    get <- function() x
    
	# The function "setinv", stores matrix's inverse
    setinv <- function(inv) i <<- inv
	
	# The function "getinv", returns matrix's inverse 
    getinv <- function() i
    
	# Create a special matrix of dim 1 x 4 (1 row and 4 column) which is 
	# capable of caching the inverse of a matrix. The matrix is initialised
	# with set, get, setinv, and getinv. 
	# The matrix 
	m <- cbind(set = set, get = get,
               setinv = setinv, getinv = getinv)
    # The special matrix m is return
			   
}


## Write a short comment describing this function
## x - It is special matrix object that caches the inverse of the matrix


## The function cacheSolve take x which is the special matrix returned
## by function makeCacheMatrix. The function returns the inverse of the 
## matrix. The special matrix is used to get a cached inverse or else 
## inverse is calculated using the solve function, and cached so that next
## time, the cache value is returned.
## It is assumed that the matrix is invertible 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x[1,]$getinv()
    if(!is.null(i)) {
        message("Getting cached inverse")
        return(i)
    }
    
	# Get the matrix
	data <- x[1,]$get()
        
	# it is assumed that matrix is invertible i.e., solve would not throw an 
	# error. In production we have to check if the matrix is invertible and only
	# then proceed to calculte the inverse.
	i <- solve(data, ...)
	
	# Cache the inverse so that next time, the cached value is returned
    x[1,]$setinv(i)
    i
}
