## Computing the inverse of a matrix is an expensive computational task.
## Whenever possible, we would like to not calculate the inverse of a matrix twice.
## This can be accomplished by storing the inverse of a matrix in the system cache.
## The following functions cache the value of a newly inverted matrix for future 
## retrieval.
## For matrices that have already been inverted, the function will access the inverse
## from the cache.


## makeCacheMatrix creates a list from functions that allows the user to:
## 1. Set the value of the matrix x.  x$set()
## 2. Retrieve the value of matrix x. x$get()
## 3. Set the inverse of matrix x. x$setinverse(inverse)
## 4. Retreive the inverse of matrix x. x$getinverse()

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse1) inverse <<- inverse1
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes the output of makeCacheMatrix and checks to see if the inverse
## has already been calculated. If so, it retreives the inverse value from the 
## cache and returns it as a matrix. If the inverse has not yet been 
## calculated, then cacheSolve will set calculate the inverse of the input matrix 
## set the inverse for the particular matrix in the makeCacheMatrix list, and 
## return the inverse of the matrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  inv
}

x = rbind(c(1,-.25,1), c(-.25,1,.25),c(.25,1,-.25))
m = makeCacheMatrix(x)
m$get()


system.time(cacheSolve(m))
system.time(cacheSolve(m))
