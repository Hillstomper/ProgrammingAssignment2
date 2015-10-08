## Two functions in this file:
## Given a matrix makeCacheMatris creates a special matrix that can store the 
## inverse of itself.
## cacheSolve returns the inverse of the given matrix without calculating it if
## the value is already stored.



## makeCacheMatrix - The function creates four functions linked to stored values
##                   of the given matrix and its inverse, two functions each.
##                   The function returns a list with the four functions.

makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inver) inv <<- inver
      getinverse <- function() inv
      list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: Takes as argument a special matrix created by the function
##             makeCacheMatrix and return the inverse of the matrix.
##             First it checks if the inverse of the matrix is stored and
##             return it directly in that case, otherwise calculate, store and
##             return the inverse.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if (!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinverse(inv)
      inv
}
