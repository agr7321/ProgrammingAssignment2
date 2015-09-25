## Together, these two functions will generate an inverse matrix, save the result to cache   
## and recall the result.

## The makeCacheMatrix contains four functions, set, get, setmatrix, getmatrix. To determine , 
## the inverse matrix, first assign a matrix and then call the function.
## For example, you can generate a simple matrix with: a<-matrix (C(4,3,3,2), 2, 2)
## and then use b<-makeCacheMatrix(a) to run the function on the matrix.

makeCacheMatrix <- function(x = matrix()) {
	

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## For cacheSolve, the function checks to see if the inverse matrix has been generated.
## If not, it solves the matrix.  For example, you can run the function c<-cacheSolve(b) 
## and then print c to see the inverse matrix.

cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
