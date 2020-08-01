##makeCacheMatrix is a function that creates 
## a special object that stores a matriz and
## catches its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) mat_inv <<- inverse
  getInverse <- function() mat_inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve will get the inverse of the matrix created in the previous 
## function (makeCacheMatrix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getInverse()
  if (!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  mat <- x$get()
  mat_inv<- solve(mat, ...)
  x$setInverse(mat_inv)
  mat_inv
}