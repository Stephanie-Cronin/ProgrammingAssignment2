## makeCacheMatrix contains 4 functions that are then put into a list, with an input "x" which should be a matrix.
## cacheSolve calculates the inverse of makeCacheMatrix

#makeCacheMatrix does the following in order and puts them in a list.
# set the matrix
# get the matrix
# set the inverse of the matrix
# get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) i <<- inverse
  getinverse<-function()i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##computes the inverse of the matrix made by makeCacheMatrix, if it hasn't already been computed and returns the result 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

