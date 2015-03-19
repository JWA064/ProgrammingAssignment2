## This function creates a special "matrix" object than can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #sets x equal to an empty matrix
  I<-NULL
  #set the inverse equal to NULL
  set<-function(y){
    x<<- y
    #set function assign the argument to x
    I<<- NULL
    #Once the set function is called, Inverse is re-set to NULL
  }
  get<- function()x
  #get function returns the matrix
  
  setInverse<- function(solve) I<<-solve
  #setInverse overrides the previous I value and assign the argument to Inverse
  
  getInvers<- function()I
  #getInverse returns the Inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse= getInverse)
  #create a list of the functions
  
}


## Cachesolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  I<- x$getInverse()
  #Retrives the most recent value for the inverse
  
  if(!is.null(I)){
    message("getting cached data")
    return (I)
    #If the Inverse value is not NULL (was previously calculate), cacheSolve returns that value
  }
  #If the Inverse value is NULL, then you retrive matrix x and calculate the Inverse with solve() function
  message(" newly calculating data")
  data<- x$get()
  I<-solve(data, ...)
  x$setInverse(I)
  #Set Inverse to the newly calculated value
  I # Returns the new Inverse value

      
}
