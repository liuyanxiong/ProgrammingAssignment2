## This function created the inverse of the matrix and 
## if the inverse of the matrix has already been calculated 
## previously, the function will directly pull the result 
## of the cached inverse matrix

## First function create a list for setting the matrix, 
## getting the matrix setting the inverse and getting the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(solve) m<<- solve
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## Second function compute the value for the first one

cacheSolve <- function(x, ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
## use k<-makecache() to create the list
## use k$set(z) to store the matrix z you desire
## use cachesolve(k) to compute the result
