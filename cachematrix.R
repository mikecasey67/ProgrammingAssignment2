## makeCacheMatrix and cacheSolve are functions that allow one to create
## a special "matrix" object from a matrix, compute the inverse of the
## matrix once and then recall the cached inverse of that matrix rather
## than recomputing it each time the result is needed.

## makeCacheMatrix is a function that creates a "matrix" object that
## allows the inverse of a matrix to be cached, assuming the matrix is
## square and has an inverse.  makeCacheMatrix returns a list of functions.
## Example:
## If we want this "matrix" object to be called myMatrixObject, and we
## want it to be based on a square matrix with an inverse called myMatrix,
## then the proper use of makeCacheMatrix would be:
## myMatrixObject <- makeCacheMatrix(myMatrix)

makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setMatrixInverse <- function(solve) mInv <<- solve
  getMatrixInverse <- function() mInv
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## cacheSolve is a function that returns a matrix that is the inverse of
## a matrix that has been processed by the makeCacheMatrix function.  The
## result of the makeCacheMatrix function is passed as the parameter to
## cacheSolve.
## Example: (continuing from the example in the makeCacheMatrix comment)
## cacheSolve(myMatrixObject)  ## returns the inverse of myMatrix
## To test that it is correct, the following should return the identity
## matrix (up to some numerical round-off error)
## myMatrix %*% cacheSolve(myMatrixObject)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInv <- x$getMatrixInverse()
  if(!is.null(mInv)) {
    message("getting cached data")
    return(mInv)
  }
  data <- x$get()
  mInv <- solve(data, ...)
  x$setMatrixInverse(mInv)
  mInv
}
