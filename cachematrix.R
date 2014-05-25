## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #call $set(y) store the matrix y into x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #call $get() output the matrix which is stored in x
  get <- function() x
  #$setinverse(inverse) store the inverse matrix inverse into m 
  setinverse <- function(inverse) m <<- inverse
  #$getinverse() output the inverse matrix m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #check whether the inverse matrix has be computed
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #if not compute the inverse matrix by solve function
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

}



