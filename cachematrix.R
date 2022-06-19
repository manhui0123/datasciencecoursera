## Write a short comment describing this function

## Creates a special matrix object which is for caching its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse Object
  inv <- NULL
  ## Setup the matrix
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  ## Get the matrix
  get <- function() x
  ## get the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  ## Return a list of inverse matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## Return the inverse matrix
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Calc the inverse 
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}

##----------------------Check the program--------------------------------
testMatrix <- matrix(seq(1,4),2,2)
testInvMatrix <- cacheSolve(makeCacheMatrix(testMatrix))
testInvMatrix
