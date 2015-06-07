## cachematrix file is to cache the inverse of a matrix
## It stores functions:
##  makeCacheMatrix() and cacheSolve()

## makeCacheMatrix(): creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  
}


## cacheSolve(): computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x = maxtrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- matrix(data, ...)
  x$setmatrix(m)
  m
}
