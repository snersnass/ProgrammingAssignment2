## Use makeCacheMatrix to store your matrix
## When you call cacheSolve, instead of sending in the matrix only,
##  send in the makeCacheMatrix variable.
## example call 
##    myCacheObject <- makeCacheMatrix(matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), nrow=3, ncol = 3))
##    cacheSolve(myCacheObject)
##    cacheSolve(myCacheObject)

##    If you aren't sure if your matrix is invertable, then make your matrix 
##      det(matrixName)
##      the result must not be 0. If it is 0, then you will get an error
##      the numbers in the example call are inversible

## Create a special matrix object that can cache its invers
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set the value of the matrix
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse
  setinverse <- function(solve) m <<- solve
  
  ## get the value of the inverse
  getinverse <- function() m
  
  ## assign the names after the $
  list(set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)
}


## compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## if the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## call the makeCacheInverse
  m <- x$getinverse()
  
  ## if it is not null, then it is cached
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if you get here, then it is null, so get it calculated
  data <- x$get()
  m <- solve(data, ...)
  
  # set the value of the mean for future use
  x$setinverse(m)
  m
}
