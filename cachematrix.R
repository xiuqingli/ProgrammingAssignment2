## This function creates a speical "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #initial the valuable inverse
  inverse <- NULL
  mymatrix <<- x
  set <- function(x){
    mymatrix <<- x
    inverse <<- NULL;
  }
  get <- function() mymatrix
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
  # get inverse
  inverse <- m$getinverse()
  # check whether inverse is computed, if TRUE return the it and end function.
  if(!is.null(inverse)){
    message("Getting cached data...")
    return(inverse)
  }
  #get the matrix
  mtx <- m$get()
  #compute the inverse
  inverse <- solve(mtx)
  m$setinverse(inverse)
  inverse
}