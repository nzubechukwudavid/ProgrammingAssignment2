## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache 


makeCacheMatrix <- function(x = matrix()) 
{
  # initialize the matrix to NULL
  inverse  <- NULL
  set <- function(y) 
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() 
  {
    x
  }
  
  setinverse <- function(inv)
  { 
    inverse <<- inv
  }
  getinverse <- function()
  {
    inverse
  } 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) 
{
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) 
  {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

test <- function() 
{
  base_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
  base_matrix$get()
  base_matrix$getinverse()
  cacheSolve(base_matrix)
  base_matrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modifies the existing matrix
  cacheSolve(base_matrix)   # this Computes, caches, and returns new matrix inverse
  base_matrix$get()         # Returns the matrix
  base_matrix$getinverse()  # Returns the matrix inverse    
  base_matrix$get() %*% base_matrix$getinverse() # returns the identity matrix
}

