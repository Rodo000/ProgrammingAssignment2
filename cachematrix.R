## these functions demonstrate the concept of caching in R. Together, the cache the inverse of a given matrix 
## to avoid recalculating the inverse for the same matrix 

## the function 'makeCacheMatrix' creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_matr <- NULL
  set <- function(y) {
    x <<- y
    inv_matr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_matr <<- inverse
  getinverse <- function() inv_matr
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## the function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##it retrieves the inverse from the cache if it has already been calculated (and the matrix not changed).

cacheSolve <- function(x, ...) {
  inv_matr <- x$getinverse()
  if(!is.null(inv_matr)) {
    message("getting inverse matrix")
    return(inv_matr)
  }
  matrix_ <- x$get()
  inv_matr <- solve(matrix_, ...)
  x$setinverse(inv_matr)
  inv_matr

        ## Return a matrix that is the inverse of 'x'
}
