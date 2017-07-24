## makeCacheMatrix creates a list of functions to set and get the value of a matrix
## and set and get the value of the inverse of a matrix
##
## cacheSolve calculates the inverse of the matrix created in makeCacheMatrix
## if the inverse is in cache, then it retrieved
## if the inverse is not in the cache, then it is created in the working environment
##
## 

makeCacheMatrix <- function(x = matrix()) {
  ## Creates a list containing functions to:
  ## a) set the value of the matrix,
  ## b) get the value of the matrix,
  ## c) set the value of the inverse of the matrix
  ## d) get the value of the inverse of the matrix
  
  ## initialize inv to NULL
  inv <- NULL
  
  ## set the value of the matrix in the working environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## invert the matrix and store in inv (cached)
  setinv <- function(inverse) inv <<- inverse 
  
  ## get the inverted matrix from inv (cached)
  getinv <- function() inv
  
  ## return the functions to the working environment
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
  
  }



cacheSolve <- function(x, ...) {
  ## cacheSolve calculates the inverse of the matrix created in makeCacheMatrix
  ## if the inverse is in cache, then it retrieved
  ## if the inverse is not in the cache, then it is created in the working environment
  ## if the matrix cannot be inverted, then return NA
  
  ## get the inverse of the matrix from cache
  inv <- x$getinv()
  
  ## if the inverse has already been calculated, then pull from cache
  if (!is.null(inv)){
    ## get it from the cache and skips the computation. 
    message("getting cached inverse matrix")
    
    ## return inverse
    return(inv)
  }
  
  ## if the inverse has not been calculaed and storged in cache, then get the matrix value 
  data <- x$get()
  
  ## try calculate the inverse of the matrix value
  ## if throws an error, likely due to the matrix being singular, so return NA
  
  tryCatch(
    {
        ## try to invert the matrix
        inv <- solve(data, ...)  
      
    },
    ## error handling
    error=function(er){
      message("Error, likely matrix was singular and could not be inverted")
      message(er)
      
      ## return NA
      return(NA)
    }
    
    
  )
  
  
  ## set the value of the inverse in the working environment 
  x$setinv(inv)
  
  ## return the inverse value
  return(inv)
}
