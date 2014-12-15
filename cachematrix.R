## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that uses object oriented programming
## to create a class called "CacheMatrix" that has methods to "get" the created CacheMatrix
## and to calculate and "get" the CacheMatrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
              i <- NULL
              
              set <- function(y){
                x <<- y
                i <<- NULL
              }
              
              get <- function() x
              setInverse <- function(solve) i <<- solve
              getInverse <- function() i
              list (set = set, get = get, setInverse = setInverse, 
                    getInverse = getInverse)
              
}


## Write a short comment describing this function
## cacheSolve uses the getInverse method to retrieve x's inverse matrix.
## If x does not have cached data then the function creates uses the methods in 
## makeCacheMatrix to create a matrix and "set" its inverse after the solve function is used
## to calculate matrix x's inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getInverse()
          
          if(!is.null(i)) {                  
              message("getting cached data") 
              return(i)                     
          }
          
          data <- x$get()
          i <- solve(data, ...)
          x$setInverse(i)
          i          
}
