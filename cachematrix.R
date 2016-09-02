## Put comments here that give an overall description of what your
## functions do

## 'makeCacheMatrix' creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mati <- NULL ##begins by setting the inverse to NULL as a placeholder for a future value
  set <- function(y) {
    x <<- y
    mati <<- NULL 
  } ##defines a function to set the matrix, x, to a new matrix, y, and resets the solve, mati, to NULL
  get <- function() x ## returns the matrix, x
  setsolve <- function(solve) mati <<- solve ##sets the inverse, mati, to mati
  getsolve <- function() mati ##returns the inverse, mati
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve) ##returns the 'special matrix' containing all of the functions just defined
} 


## 'cacheSolve' computes the inverse of the special 'matrix' returned by `makeCacheMatrix` or retrieves the inverse from the cache if it has been computed for that matrix before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mati <- x$getsolve()
  if(!is.null(mati)) {
    message("getting cached data")
    return(mati)
  }
  data <- x$get()
  mati <- solve(data, ...)
  x$setsolve(mati)
  mati
}
