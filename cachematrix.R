## The following two functions take a matrix and calculate it's inverse.
## The inverse is cached.



## This function creates a list to get and set the value of a matrix
## and get and set it's inverse.


makeCacheMatrix <- function(x =  matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinv <- function(inv) i <<- inv
     getinv <- function() i
     list(set = set, 
          get = get,
          setinv = setinv,
          getinv = getinv)
}


## The following function returns the inverse of the matrix
## specified above in makeCacheMatrix.  An existing cached inverse is 
## retrieved with getinv().  If the inverse does not exist then it 
## retrieves the matrix with get(), calculates the inverse,
## stores it with setinv(), and finally returns the new inverse.


cacheSolve <- function(x, ...) {
     i <- x$getinv()
     if(!is.null(i)) {
          message("getting cached matrix inverse")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i
}



