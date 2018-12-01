# Assingment 2

# Following functions help save time and memory by cahing the inverse of a matrix
# makeCacheMatrix function makes a matrix suitable to be cached and 
# cacheSolve matrix returns an inverse of a matrix but 1st it looks for it in cache
# if it is not found in cache the we compute inverse with the help of solve()
# thus saving time and memory as matrix inverse is costly computation

# Making a Special matrix for inverse operation
makeCacheMatrix <- function(x = matrix()){
  iMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    iMatrix <<- NULL
  }
  
  getMatrix <- function() x                              
  setInverse <- function(inverse) iMatrix <<- inverse  
  getInverse <- function() iMatrix                     
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

# Function returns (cached) inverse (if exists) of a matrix 
cacheSolve <- function(x,...){
  iMatrix <- x$getInverse()
  if(!is.null(iMatrix)) {                       
    message("Getting Cached Invertible Matrix")     
    return(iMatrix)                             
  }
  
  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                      
  iMatrix <- solve(MatrixData, ...)             
  x$setInverse(iMatrix)                          
  return(iMatrix) 
}
