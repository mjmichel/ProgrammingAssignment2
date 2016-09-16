## The first function, makeCacheMatrix creates special "matrix"
## 1. set the value of the matrix 2. get the value of the matrix 3. set the value of the matrix

## 4. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) m <<- solve(x)
  getinverse <- function() m
  list (set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
}


## This following function return the inverse of square matrix.
## First the function check whether the matrix has been inverted. If so, it will get the invertible matrix
## Otherwise, it will invert the matrix and set the value of the matrix via cache function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached inverse")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
