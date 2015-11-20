## Generally this function creates a special "matrix" object that caches its inverse.
## This function sets the value for a matrix,gets the value for a matrix,and also sets
##and gets value for inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  matinv<-NULL    #matinv stands for matrix inverse
  set<-function(y){
    x<<-y
    matinv<<-NULL
  }
  get<-function() x
  setmatinv<-function(solve) matinv<<- solve
  getmatinv<-function() matinv
  list(set=set, get=get,
       setmatinv=setmatinv,
       getmatinv=getmatinv)
}


## This function returns the inverse of the special "matrix" created by makeCacheMatrix.
##If the inverse has already been calculated,then cachesolve function gets the inverse from the cache.

cacheSolve <- function(x, ...) {
  matinv<-x$getmatinv()
  if(!is.null(matinv)){
    message("getting cached data")
    return(matinv)
  }
  matrix<-x$get()
  matinv<-solve(matrix, ...)
  x$setmatinv(matinv)
  matinv
}
