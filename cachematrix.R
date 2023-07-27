## These functions calculate the matrix inverse
## however, they also provide the ability to 
## cache a specific matrix and subsequently 
## retrieve the result


## This function generates the framework needed to 
## cache a matrix
## e.g., mymat <- makeCacheMatrix(matrix(c(4,2,7,9),2,2)
## will generate the list of functions used for this purpose

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(solve) m<<- solve
  getinv<-function() m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function will perform the calculation and store 
## the result in the above framework. It requires as 
## input an object created with the previous function
## e.g., cacheSolve(mymat)
## If the command is re-run, will show a message to  
## indicate that the result is being retrieved

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

