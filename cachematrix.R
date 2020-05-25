#using the example given in the question and just adapting to use matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }#creates a special matrix
  get <- function() x
  #set the value of the matrix
  setinverse <- function(inverse) i <<- inverse
  #get the value of the inverse
  getinverse <- function() i
  #get the value of the inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  # i receive the inverse value
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #if i is null
  #i receive the new inverse and set in the matrix
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i 
}
