# makeCacheMatrix: This function creates a special "matrix" 
# object that can cache its inverse.

makeCacheMatrix <- function (x = matrix()) {
  # x is a matrix passed in
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function (Solve) m << Solve
  getInverse <- function() m
  matrix (set = set, get = get,
          setInverse = setInverse
          getInverse = getInverse)
}

# cacheSolve: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve < function (x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x <- x$getInverse()
  if (!is.null(m)) {
    message ("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setInverse(m)
  m
}


