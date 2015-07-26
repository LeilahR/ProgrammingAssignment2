## Put comments here that give an overall description of what your
## functions do

##Function created with null matrix as input

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse matrix as NULL
  m_inverse <- NULL
  ## Cache value of non-inverse matrix (as y)
  #Calling function
  set <- function(y) {
    x <<- y
    ## keep inverse matrix value as NULL
    m_inverse <<- NULL
  }
  
#Function to compute inverse of matrix x
  #calling function
  get <- function() x
  #Calculating inverse using solve function
  setinverse <- function(solve) m_inverse <<- solve
  #gets the inverse value just calculated
  getinverse <- function() m_inverse
  ##Creatign list of functions which will be called in function below
  ##list functions for calculating inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function for getting cache of matrix x

cacheSolve <- function(x,...) {
        ## Getting inverse value calcuated in makeCacheMatrix function above

    m_inverse <- x$getinverse()
  
    #If inverse of matrix exists, returns a message
    
  if(!is.null(m_inverse)) {
    message("getting cached data")
    return(m_inverse)
  }
    
    #If there is no inverse, it is not calculated using the solve function
  data <- x$get()
  m_inverse <- solve(data, ...)
  x$setinverse(m_inverse)
  m_inverse
  
}