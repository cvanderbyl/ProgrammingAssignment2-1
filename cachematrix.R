## This function will create a special "matrix" object that can cache its inverse.
## 
rm(list=ls())
data=matrix(c(5,1,4,2,5,6,10,12,11),3,3)

makeCacheMatrix=function(x = matrix()) {
  m=NULL#                                 Set m to an empty set in makeCacheMatrix environment
  set=function(y) {                       #Create function to set data
    x<<-y                                 #assign x to y in a higher environment
    m<<-NULL                              #assign m to null in the higher environment
  }
  get=function(){x}                       #Create function get to return x
  setinverse=function(solve){m<<-solve}   #Set inverse function to solve by assigning the solve function to m in a higher environment
  getinverse=function(){m}                #retrive m in the function environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Utilise cachematrix to return inverse

cachesolve <- function(x, ...) {        #Set cache solve function with ability to pass arguments to correspoonding function
  m <- x$getinverse()                   #retrive getinverse function from make cache matrix function
  if(!is.null(m)) {                     #
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()                       #Retrive data from the get function in cache solve function
  m <- solve(data, ...)                 #invert the data retrieved from the cache
  x$setinverse(m)                       #Set the inverse of the cache matrix
  m                                     #Return the inverted matrix
}

cachesolve(makeCacheMatrix(data))     #Demonstration

