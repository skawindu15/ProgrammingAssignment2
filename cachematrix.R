##cachesolve function stores the value of the inverse of matrix.then it returns value when the function is called.

## define the functions to get,get inverse and set inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    n <- NULL                               
    set <- function(y) {
    x <<- y
    n <<- NULL
  }
  
  get <- function() x                        
  setinverse<-function(inverse){n<<-inverse} 
  getinverse<-function(){n}                 
  
  list(set = set, get = get,                
       setinverse = setinverse,
       getinverse = getinverse)

}


##If there is a value for inverse of the matrix, then this function returns it to the makeCacheMatrix function


cacheSolve <- function(x, ...) {
        n<-x$getinverse()                   
  
  if(!is.null(n)){
      message("getting cached data")    
      return(n)                         
  }
  
  data<-x$get()                     
  n<-solve(data, ...)               
  x$setinverse(n)                  
  n
}
