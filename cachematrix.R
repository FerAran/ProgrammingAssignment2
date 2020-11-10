## The first function (makeCacheMatrix) creates a special "matrix" than can save the cache of 
#  its inverse whichs avoids costrly computations. 

makeCacheMatrix <- function(x = matrix()) {
  
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inversa <<- inverse
  getInverse <- function() inversa
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
  

}


## This function uses the matrix created with the makeCacheMatrix. If the matrix reamins the same
## and has already been calculated, it takes the inverse from the cache instead that calculating
## it again and rettns the inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inversa <- x$getInverse()
  if (!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  mat <- x$get()
  inversa <- solve(mat, ...)
  x$setInverse(inversa)
  inversa
}
