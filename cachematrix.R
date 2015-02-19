## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #provides a default if cacheSolve has not yet been used
    set <- function(y) { #set the value of the matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse # chache's the inverse
    getinverse <- function() i # returns the inverse
    list(set = set, get = get, # creates a list to store the four functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse from makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse() # if an inverse has already been calculated this gets it
    if(!is.null(i)) { # If matrix exists, prints a text message and returns the cached matrix
        message("getting cached data")
        return(i)  ## Returns a matrix that is the inverse of 'x'
    }
    data <- x$get() # Get the matrix and store in data 
    i <- solve(data, ...) #calculates and stores inverse of matrix
    x$setinverse(i) # Cache the inverse
    i # return the inverse
       
}
