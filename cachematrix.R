## Put comments here that give an overall description of what your
## functions do
## This function creates a special "matrix" object that can cache its inverse

## Write a short comment describing this function
## The following function calculates the mean of the special "matrix" created with the above function
## However, it first checks to see if the inverse has already been calculated
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setinverse function.

makeCacheMatrix <- function(x = matrix()) {
  
  set <- function(y) {  ## define the set function to assign new 
    x <<- y ## value of matrix in parent environment
    inv <<- NULL ## initialize inv as NULL
  }
  get <- function() x ## define the get fucntion - returns value of the matrix 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, ## it calculates the inverse of the data 
       setinverse = setinverse, ## and sets the value of the mean in the cache via the setinverse function
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## Computing the inverse of a square matrix can be done with the solve function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() ##the inverse is assigned to inv
  if(!is.null(inv)) { ## if function is used to get the value if not null
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinverse(inv)
  inv      ## Return a matrix that is the inverse of 'x'
}
}
