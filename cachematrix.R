## A brief about the function
## set and get functions perform the same task 
## setinv and getinv are similar in function as of setmean and getmean
## Proper way of testing is to create an object first and use it in function
## Follow the steps below for a sample case
## amatrix <- makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2), nrow =3, ncol = 3))
## cacheSolve(amatrix)
## amatrix$get()
## amatrix$getinv()
## x$get and x$getinv can't be used. Why???

library(matlib)
makeCacheMatrix <- function(x = matrix()) {
  if(ncol(x)==nrow(x) && det(x)!=0){
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(mat) m <<- mat
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
    }
}else{
  return(message("This matrix is not invertible"))
}
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
