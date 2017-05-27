## Put comments here that give an overall description of what your
## functions do
# Caching Inverse of a Matrix
#
# Example usage
# source('C:/DataScience/Course2/week3/cachematrix.R')
## x <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2, byrow = TRUE)
## x2 <- makeCacheMatrix(x)
## cacheSolve(x2)
## Inverse Matrix was created
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## cacheSolve(x2)
## Inverse was cached
## [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
## ----------------------------
### Example 2 --> 3 x 3 matrix
### y <- matrix(c(7,2,1,0,3,-1,-3,4,2), nrow = 3, ncol = 3, byrow = TRUE)
### y2 <- makeCacheMatrix(y)
### cacheSolve(y2)
### Inverse Matrix was created
### [,1] [,2]        [,3]
### [1,] 0.11764706  0.0 -0.05882353
### [2,] 0.03529412  0.2  0.08235294
### [3,] 0.10588235 -0.4  0.24705882
### cacheSolve(y2)
### Inverse was cached
### [,1] [,2]        [,3]
### [1,] 0.11764706  0.0 -0.05882353
### [2,] 0.03529412  0.2  0.08235294
### [3,] 0.10588235 -0.4  0.24705882


#
# Creates a matrix that can cache it's inverse
#
# Args:
#   x: A matrix (Optional)
#
# Returns:
#   A matrix with functions to get/set value & get/set inverse
#



makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inv <- NULL
  
  ## getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  ## getter/setter for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
    
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
    
}


# Computes the inverse of a matrix. If the inverse has already been
# calculated before, the cached inverse is returned.
#
# Args:
#   x: A matrix
#   ...: Extra arguments
#
# Returns:
#   The inverse of the matrix
#

cacheSolve <- function(x, ...) {

  inv <- x$getinv()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inv)) {
    message("Inverse was cached")
    return(inv)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache the newly inverse matrix computed
  x$setinv(inv)
  message("Inverse Matrix was created")
  # return inverse of matrix of x
  return(inv)
}
