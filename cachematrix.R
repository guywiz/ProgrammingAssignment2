
## The following two functions are used to create a special object that stores a matrix and cache's its inverse

## The makeCacheMatrix function sort of wraps the given matrix into a list of functions
## allowing to either set or get the initial matrix, and eventually set of get its inverse.
## When setting the value of the matrix, any previously computed inverse is reset to NULL
## allowing the inverse to be recomputed.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(n) {
    m <<- n
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function is responsible for actually computing the inverse
## of the matrix *if it has not yet been computed*. Otherwise, it retrieves it.
## If computed the inverse is then cached (saved permanently) using the setinverse function
## wrapped with the matrix (using the previous function).

cacheSolve <- function(m) {
  ## Return a matrix that is the inverse of 'm'
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrixData <- m$get()
  inv <- solve(matrixData)
  m$setinverse(inv)
  inv
}

## Typical use of these functions is
# > wrappedMatrix <- makeCacheMatrix()
# > wrappedMatrix$set(m) -- where m is a matrix object
# > cacheSolve(wrappedMatrix)
## calling it a second time will show the cached value is used
# > cacheSolve(wrappedMatrix)
