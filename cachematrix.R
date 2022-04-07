## The functions below calculates the inverse of a matrix and caches the result for retrieval 
##when using the same data in several computations helping to save time

## The function below catches the matrix that is to be used often 

makeCacheMatrix <- function(x = matrix()) {   #defines argument as a default empty matrix
              inv <- NULL                     #creates object of inverted matrix to be used later, sets value to NULL
              set <- function(y) {            #creates set function which sets the value of the matrix equal to the value of x
                x <<- y                       #function defined as 'y' as x has already been defined earlier
                inv<<- NULL
              }
              get <- function() x                           #retrieves the x matrix in the parent environment
              setinv <- function(inverse) inv <<- inverse   #assigns input argument to 'inv' variable in parent environment through the '<<-' 
              getinv <- function() inv
              list(set = set, get = get,                    #creates a list of named variables that can be easily called in the next function using the "$" sign
                   setinv = setinv,
                   getinv = getinv)
}


## The function below retrieves the inverse of a matrix or computes it if not

cacheSolve <- function(x, ...) {
                                       ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {                 #checks if the matrix had been cached i.e. the inv object is not null
    message("retrieving cached data") #shows message when matrix is already cached and is being retrieved
    return(inv)                      # returns cached result of inverted matrix
  }
  data <- x$get()                  #for a matrix that is being run for the first time this runs the solve function to return inverse of matrix 
                                  #defined in parent environment and caches it
  inv <- solve(data, ...)
  x$setinv(inv)
  inv            #returns the inverted matrix
}
