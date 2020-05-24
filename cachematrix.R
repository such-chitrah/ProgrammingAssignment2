

## Sets and stores the matrix and cached inverse matrix

makeCacheMatrix <- function(x = matrix())
{
  i<-NULL
  #Setting the value of a invertile square matrix
  setmatrix <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  #Getting the value of the matrix 
  getmatrix <- function() x
  #Setting the inverse of the matrix (caches the inverse matrix)
  setinverse <- function(inverse) i <<- solve
  #Getting the inverse of the matrix (Used to access the cache matrix)
  getinverse <- function() i
  #listing all the matrices
  list(setmatrix= setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Checks if the inverse is present in the cache, if not, then calculates and returns the inverse of a matrix

cacheSolve <- function(x, ...) {
  #Getting the inverse matrix
  i <- x$getinverse()
  #Checking the if the cached inverse matrix x exists 
  if(!is.null(i)) {
    message("getting cached data")
  # If yes, returns the cached matrix x
    return(i)
  }
  data <- x$getmatrix()
  #Calculating inverse of the matrix x
  i <- solve(data, ...)
  #storing the inverse matrix x to cache
  x$setinverse(i)
  #returning the inverse matrix of x
  i
}

