## Coursera Programming in R assignment 2.
## needs the MASS package for the solve function. 
## 


## This function creates a special "matrix" object that can cache 
## its inverse.
## Similar to the example given for Vectors

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL;
  # Setter function
  set <- function(matrix) {
    x <<- matrix
    xInverse <<- NULL
  }
  # Getter function - return the input x (matrix())
  get <- function() x
  
  # Setter function for the inverse matrix
  setInverse <- function(inverse) {
    xInverse <<- inverse
  }
  
  # Getter function for the inverse matrix
  getInverse <- function() {
    xInverse
  }
  
  list(set = set,  get = get, 
       setInverse = setInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(y, ...) {
  ## Return a matrix that is the inverse of 'y'
  m <- y$getInverse()
  if (!is.null(m)) {
      message("getting cached data")
      return (m)
  }
  ## matrix not in cache so no calculate it.
  data <- y$get()
  # solve this using MASS package solve function
  m <- solve(data)
  #Put the result back into the cache
  y$setInverse(m)
  # return the result
  return(m)
}


## Test the functions above
test1 <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = TRUE)
testCached <- makeCacheMatrix(test1)
result1 <- cacheSolve(testCached)
result1 <- cacheSolve(testCached)


test2 <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE)
testCached2 <- makeCacheMatrix(test2)
result2 <- cacheSolve(testCached2)
result2 <- cacheSolve(testCached2)

                    

