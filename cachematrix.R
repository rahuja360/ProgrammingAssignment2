## Write a pair of functions that cache the inverse of a matrix

## First function sets the value of a matrix, gets the value of the matrix,
## sets the value of the inverse, and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function()m
  list(set = set, 
       get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Second function calculates the inverse of the matrix. First checks if the inverse has already been calculated
## If so, gets inverse from cache. Otherwise, calculates inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data!")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}

## Examples of function working
B <- matrix(c(7,0,-3,2,3,4,1,-1,-2),3,3)
print(B)
B1 <- makeCacheMatrix(B)
cacheSolve(B1)
#Solve with cached data
cacheSolve(B1)
