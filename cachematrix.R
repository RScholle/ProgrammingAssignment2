## This first function, `makeCacheMatrix` creates a list 
## containing functions to
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {       ##Sets the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x       ##Gets the value of the matrix
  setInverse <- function(solve) m <<- solve   ##Sets the value of the inverse matrix
  getInverse <- function() m                  ##Gets the value of the inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



##The following function calculates the inverse matrix of the special 
##matrix created in the above function.  Before doing so, it first checks 
## to see if the inverse matrix has already been calculated. If so, it 
## `get`s the inverse matrix from the cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value
##of the inverse matrix in the cache via the `setinverse`function.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  ##The 'if' statement is checking to see if inverse matrix cached already 
  if(!is.null(m)) {   
    message("getting cached inverse matrix")
    return(m)
  }
  ##If the inverse the inverse matrix is not already chached, it is 
  ##then computed and stored
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}

  