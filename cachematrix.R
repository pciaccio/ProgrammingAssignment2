## Programming Assignment 2
## The proceeding functions cache the inverse of a given matrix

## creates a matrix object that can cashe the inverse of 'x'

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){ ## sets the matrix
    x<<-y
    inv<<-NULL
  }
  get<-function() x ## gets the value of the cached matrix
  setinv<-function(solve) inv<<-solve ## sets the inversed matrix
  getinv<-function() inv ## gets the value of the cached inversed matrix
  list(set=set,get=get,setinv=setinv,getinv=getinv) 
}

## computes the inverse of the matrix returned by make CacheMatrix()
## if the inverse has already been calculated (with no change to the
## matrix), then the inverse will be retrieved from the cache

cacheSolve<-function(x,...){
  ## Return a matrix that is the inverse of 'x'

  ## returns the cached inverse if the matrix has not been changed
  inv<-x$getinv()
  if (!is.null(inv)){ 
    message("getting cached inverse")
    return(inv)
  }
  
  ## calculates the inverse of the matrix if it has not already been calculated
  data<-x$get() 
  inv<-solve(data,...)
  x$setinv(inv)
  
  ## Return a matrix that is the inverse of 'x'
  return(inv)
}
