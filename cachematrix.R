## Function to create matrix cache
## RMuzy Coursera Class

makeCacheMatrix <- function(original.matrix = matrix()) {
  
  # Check that a matrix has been given
  if (!is.matrix(original.matrix)) {
    stop("Please enter a valid matrix")
  }
  
  # Initalize variable
  inverted.matrix <- NULL
  
  set <- function(y) {
    original.matrix <<- y
    inverted.matrix <<- NULL
  }
  
  get <- function() original.matrix
  # Inversing the matrix 
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}

## Compute the inverse 
cacheSolve <- function(cacheable.matrix, ...) {
  inverted.matrix <- cacheable.matrix$get.inverse()
  
  if(!is.null(inverted.matrix)) {
    message("Getting cached matrix")
    return(inverted.matrix)
  }
  matrix.to.inverse <- cacheable.matrix$get()
  inverted.matrix <- solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inverted.matrix)
  inverted.matrix 
}