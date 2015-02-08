## This example has two functions - 
## 1. The first function saves the matrix and its inverse and
## 2. The second function gets or updates the matrix saved in the first function 
## or shows the cached or freshly calculated inverse of this matrix.

## Example usage of these methods in the R Console:
##  x<-makeCacheMatrix(matrix(1:4,2,2))
##  > x$get()
##       [,1] [,2]
##  [1,]    1    3
##  [2,]    2    4
##  > cacheSolve(x)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  > cacheSolve(x)
##  getting cached inverse
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  
##  
##  > x$set(matrix(5:8,2,2))
##  > cacheSolve(x)
##       [,1] [,2]
##  [1,]   -4  3.5
##  [2,]    3 -2.5
##  > cacheSolve(x)
##  getting cached inverse
##       [,1] [,2]
##  [1,]   -4  3.5
##  [2,]    3 -2.5


## Function makeCacheMatrix:
## The first function "makeCacheMatrix" creates a matrix as per the parameters
## passed in the argument to this function by the user. 
## Along with the matrix,this function also creates a list of functions 
## through which this matrix can be accessed and updated by an outside function 
## and its inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve:
## The second function "cacheSolve" is passed a special matrix object created
## in the first function as an argument. This function first tries to find
## the cached inverse of this matrix in the first function. If the cached inverse
## is null, then it returns a freshly calculated inverse using the built-in
## solve function that calculates inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
