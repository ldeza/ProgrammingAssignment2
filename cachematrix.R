## The script cachematrix.R contains 2 functions. 
## The first function creates a special matrix object that caches it's inverse. 
## The second function calculates the inverse of a matrix and can possibly return a cached result.

## The makeCacheMatrix function takes a matrix as an input. It returns a special matrix object that 
## contains the elements of the matrix and a list of 4 functions.
## 1. set the elements of the matrix
## 2. get the elements of the matrix
## 3. set the inverse of the matrix
## 4. get the inverser of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function takes the special matrix object created by the makeCacheMatrix function as an input 
## and returns it's inverse. However, it checks to see if the inverse has already been calculated and returns
## the cached results and skips computation which can be costly when dealing with a large matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
