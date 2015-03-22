## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Return a list of functions to:
  ## set the value of the matrix
  ## get the value of the matrix
  ## set the value of the inverse matrix
  ## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) { 
  ## Store the cached inverse matrix
  value <- NULL
  
  ## Set value of the matrix
  set <- function(y) {
    x <<- y
    value <<- NULL
  }
  
  ## Get value of the matrix
  get <- function() x
  
  ## Set the value of the inverse matrix
  setmatrix <- function(matrix) value <<- matrix
  ## Get the value of the inverse matrix
  getmatrix <- function() value
  
  ## Return the matrix with the defined functions
  list(set = set, get = get, 
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## Write a short comment describing this function

## cacheSolve: If already computed (cached), return the matrix 
## else computes the inverse of the matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  value <- x$getmatrix()
  if(!is.null(value)) {
    ## If the inverse is already calculated, return it
    message("getting cached data")
    return(value)
  }
  ## Calculate the inverse of the matrix
  data <- x$get()
  value <- solve(data, ...)
  ## Cache the inverse
  x$setmatrix(value)
  ## Returns the matrix
  value
  
}