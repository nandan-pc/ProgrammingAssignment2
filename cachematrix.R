## There are two functions 
## makeCacheMatrix - this function returns a special vector for matrix object whose invese is computed.  
## cacheSolve      - This function returns the caches inverse of the matrix if already inverse of matrix is computed. 
##                   Else computes the inverse and returns the inverse of matrix

##FunctionName  : makeCacheMatrix
##Purpose  : function makeCacheMatrix returns special vector which can cache inverse of invertable matrix
##set - set the value of the matrix
##get -  the value of the matrix
##setInverse - set the  Inverse of matrix
##getInverse - get Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL 
  
  #function to set the special Matrix 
  set <- function(matrixValues){
    matEliments <<- matrixValues                        #get the values from the vector x defined outside present environment
    size <- length(matEliments)/2            #get the dimention of the square matrix 
    x <<- matrix(matEliments,nrow=size,ncol=size) #create  matrix
    
    inv <<- NULL                            #set inverse of the matrix to null 
  }
  
  #function to get the matrix 
  get <- function() x                       
  
  #function to set the inverse matrix
  setInverse <- function(inverse) inv <<- inverse
  
  #function to get the inverse of matrix
  getInverse <- function() inv
  
  #creating the list of functions for special Matrix object 
  list(set = set(matrixValues), 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


#Function Name : cacheSolve.r
#Purpose  : function caches if the inverse of the matrix is already set. Else it computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInvers()                      #get inverse of the speical matrix object
  if(!is.null(inv)){                        #if inverse of the matrix already present then cache the result
    message("getting cached inverse")
    return(inv)
  }
  
  
  data <- x$get()                           #get the matrix for which inverse is to be computed.
  
  inv <- solve(data)                        #get the inverse of the matrix
  
  x$setInverse(inv)                         #set the inverse of the special matrix object
  inv                                       #return inverse of matrix 
}

