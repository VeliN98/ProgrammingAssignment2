## The functions below are used to cache the inverse of a matrix (having in mind that
##for this assignment the matrix is always invertible)

## The first function creates a list containing the functions to set / get the value of a matrix
## and also to set/get the inversed value.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function checks first if there is a cached value of the inverse of the matrix.
## If there is, it will print the message "getting cached data" and the value of the inverse
## If !is.null(inv) is FALSE then the function will get the data and using the function 
##Solve will calculate the inverse of the matrix. After that it will store it in setinverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Testing the functions

MyMatrix <- matrix(c(1:4), 2, 2)
CachedMatrix <- makeCacheMatrix(MyMatrix)
CachedMatrix$get()
CachedMatrix$getinverse()
cacheSolve(CachedMatrix)
CachedMatrix$getinverse()
