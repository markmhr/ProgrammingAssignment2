
## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse. Does not compute the inverse!
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getmatrix <- function() x
  
  setinverse <- function(input_inv) inv <<- input_inv
  getinverse <- function() inv
  
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Has cacheSolve already been run, an inverse calculated, and the old matrix = the current one?
  inversematrix <- x$getinverse()
  if(!is.null(inversematrix)){ 
    message("Getting cached inverse")
    return(inversematrix)
  }
  
  ## Return a matrix that is the inverse of 'x', and input it into the cache
  else {
    data <- x$getmatrix()
    
    if (det(data) == 0) {
      message("Input matrix does not have an inverse")
    } ## Check the matrix can be inverted, or an error would be thrown by solve(data)
    else {
      inversematrix <- solve(data) 
      x$setinverse(inversematrix)
      inversematrix
    } ## Invert the matrix, input to the cache, then output
  }
}