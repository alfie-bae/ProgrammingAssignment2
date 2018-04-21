## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and returns a "special"
# matrix object. This "special" matrix is a list containing
# functions that will generate and cache an inverse matrix
# if no new matrix is supplied. 

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  
  set <- function(y){
    
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  list(set = set,
       get=get,
       setinverse=setinverse,
       getinverse = getinverse
       )
  

}


## cachesolve uses the functions stored in our "special"
# matrix object to either generate an inverse matrix or
# use the inverse that's cached in memory. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }

# generate a dummy square matrix for testing our functions. 
test_matrix <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)

# test functions.
test_matrix.special <- makeCacheMatrix(test_matrix)
test_matrix.inverse <- cacheSolve(test_matrix.special)
