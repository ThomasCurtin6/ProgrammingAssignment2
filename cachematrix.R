## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

 makeCacheMatrix <- function(x = matrix()) 
      {
        inverse <- NULL
        set <- function(y) 
           {
           x <<- y
           m <<- NULL
           }
                get <- function() x
                setinv <- function(inv) inverse <<- inv
                getinv <- function() inverse
                list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
      }  

## Write a short comment describing this function

 cacheSolve <- function(x, ...) {
     
           matrx <- x$getinv()
           if(!is.null(matrx)) {
                message("got some cached values")
                return(matrx)
           }
           data <- x$get()
           matrx <-solve(data, ...)
           x$setinv(matrx)
           return(matrx)
      }
     
     
             ## Return a matrix that is the inverse of 'x'
     
 test = function(matrx){
      ## @mat: an invertible matrix
      
      temp = makeCacheMatrix(matrx)
      start.time = Sys.time()
      cacheSolve(temp)
      took = Sys.time() - start.time
      print(took)
     
      start.time = Sys.time()
      cacheSolve(temp)
      took = Sys.time() - start.time
      print(took)
}   
     

