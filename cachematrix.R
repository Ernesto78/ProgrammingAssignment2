


##This function creates the matrix object that caches the inverse matrix

makeCacheMatrix <- function(x = matrix() {
  invx <- NULL
  
  ## This function sets the matrix
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  
  ## This function gets the matrix
  get <- function(){
    
    x
  }
  
  ## This function sets the inverse matrix
  setinverse <- function(inverse) {
    
    invx <<- inverse
  }
  
  ##This function gets the inverse matrix
  getinverse <- function() {
    invx
    
  }
  
  
  ## This returns the list of the function results
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

}


## Write a short comment describing this function

## This function either gets the inverse matrix if it exists, 
## or solves for the inverse matrix.

cachesolve <- function(x = matrix()) {
  invx <- x$getinverse()
  
  ## If the inverse was previously saved then it is returned
  
  if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
  }
  ## Here the inverse matrix is solved for then returned
  
  data <- x$get()
  invx <- solve(data, ...)
  x$setinverse(invx)
  
  invx
}


