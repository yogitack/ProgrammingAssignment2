##Define the functions that cache the inverse of a matrix 
##instead of computing it again


##create a function which creates a special "Matrix" that cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  ## initialize the value of the matrix inverse to NULL
  matrixinverse <- NULL 
  
  ##Set the value of the matrix
  set <- function(y) {                      
    x <<- y
    ## change the value of inverse of the matrix in case the matrix was changed.
    matrixinverse <<- NULL              
  }
  ## Get the value of the inverse
  get <- function() x  
  
  ##Calculate the inverse of non-singular matrix using solve() function
  setinverse <- function(solve) matrixinverse <<- solve 
  
  ##Get the inverse     
  getinverse <- function() matrixinverse 
  
  ## Pass the value of the function makeCacheMatrix        
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}

## Create a function which cache the inverse of the matrix 
##( returned by makeCacheMatrix above)

cacheSolve<- function(x, ...) {                 
  matrixinverse <- x$getinverse()
  
  ##if the inverse exists, cacheSolve cache it.
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)
  }
  ##if the inverse is not there, first it is calculated and then retrieved.
  data <- x$get()                               
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}


#Test the code
testm1<- matrix(c(1,2,3,4, -3,5,2,7,6), 3,3)
n1<- makeCacheMatrix(testm1)
cacheSolve(n1)

myMatrix_object <- makeCacheMatrix(m1)
cacheSolve(myMatrix_object)

#make use of set() instead of calling makeCacheMatrix again
testm2<- matrix(c(2/3, 4/3, 5/6, 1/2), nrow = 2, ncol = 2)
myMatrix_object$set(testm2)
cacheSolve(myMatrix_object)
cacheSolve(myMatrix_object)


