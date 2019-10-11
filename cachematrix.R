
## The following function: Set the value of the matrix, get the value of the matrix, set the value of the inverse matrix, get the
# value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
  
  
}

## The function returns the inverse of the matrix.
# Firstly checks if the inverse has already been computed. If it is already computed, it gets the 
# results and skip the computation.
# If not, the function computes the inverse, sets the calue in the cache with the set_inverse function 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$get_inverse()
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  inv		
  
}