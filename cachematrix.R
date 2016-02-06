## Creating an object to store a matrix and then cache its inverse

## This is done using 2 functions 

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Need to calculate the inverse of the matrix/ retrieve the inverse from the cache if its already been calculated 

cacheSolve <- function(x, ...) {
i <- x$getInverse()
  if (!is.null(i)) {
    message("retrieving data from cache")
    return(i)
  }
  mat <- x$get()
  i<- solve(mat, ...)
  x$setInverse(i)
  i
}
