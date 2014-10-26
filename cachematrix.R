## Function to create matrix cache

makeCacheMatrix <- function(original.matrix = matrix()) {
  
  # Check for matrix
  if (!is.matrix(original.matrix)) {
    stop("Please enter a valid matrix")
  }
  
  inverted.matrix <- NULL
  
  set <- function(y) {
    original.matrix <<- y
    inverted.matrix <<- NULL
  }
  
  get <- function() original.matrix
  # Inversing the matrix using build in solve() function in R
  set.inverse <- function(solve) inverted.matrix <<- solve
  get.inverse <- function() inverted.matrix
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}

## Computes the inverse of the cacheable matrix returned by makeCacheMatrix()
cacheSolve <- function(cacheable.matrix, ...) {
  inverted.matrix <- cacheable.matrix$get.inverse()
  # Do we have cached matrix available?
  if(!is.null(inverted.matrix)) {
    message("Getting cached inverse matrix")
    return(inverted.matrix)
  }
  matrix.to.inverse <- cacheable.matrix$get()
  inverted.matrix <- solve(matrix.to.inverse)
  cacheable.matrix$set.inverse(inverted.matrix)
  inverted.matrix 
}