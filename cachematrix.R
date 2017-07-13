

## This function generates a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<- function(z){
      x<<-z
      inv<<- NULL
    }
    
    get<- function() x
    setInverse<- function(inverse)  inv<<-inverse
    getInverse<- function()inv
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function finds the inverse of the generated matrix stored in the cache.

cacheSolve <- function(x, ...) {
        inv<- x$getInverse()
        if(!is.null(inv)) {
          return(inv)
          message("getting cached data")
          }
        temp<-x$get()
        inv<-solve(temp, ...)
        x$setInverse(inv)
        inv
}
