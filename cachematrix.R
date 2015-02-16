## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function will describe the operation to manage the matrix and its inv
## in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function
## cacheSolve: x vector with CacheMatrix functions
## return m: matrix 
## if the inv matrix is already stored in the cache, we will return it
## if the inv matrix isn't stored in the cache, we will get the matrix stored in x, 
## calculate its inv, and store it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
