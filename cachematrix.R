## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

inv.mat <- NULL
  set <- function(y) {
    x <<- y
    inv.mat <<- NULL
  }
  
  get = function() x
  setinverse = function(inverse) inv.mat <<- inverse 
  getinverse = function() inv.mat
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       
   ## Return a matrix that is the inverse of 'x'
        
     inv.mat = x$getinverse()
    
    # check if the inverse has already been calculated
    
    if (!is.null(inv.mat)){
    
    # get matrix cached to skip computation next time
    
      message("caching matrix data")
      
      return(inv.mat)
    }
    
    # else calculate the inverse
    
    matrix.bigdata = x$get()
    
    inv.mat = solve(matrix.bigdata, ...)
    
    # sets the value of the inverse in the cache using setinverse function.
    
    x$setinverse(inv.mat)
    
    return(inv.mat)
        
      
        
}



### Testing

trial = function(M){
   
    
    z = makeCacheMatrix(M)
    
    start.time = Sys.time()
    cacheSolve(z)
    elapse = Sys.time() - start.time
    print(elapse)
    
    start.time = Sys.time()
    cacheSolve(z)
    elapse = Sys.time() - start.time
    print(elapse)
  }
  set.seed(1)
  r = rnorm(25000000)
  G = matrix(r, nrow=5000, ncol=5000)
  trial(G)
  
