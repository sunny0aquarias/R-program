makeCacheMatrix <- function(x = matrix()) {       ## makes a cache matrix from a given matrix
  
      inv <- NULL     ## initialize the cache Matrix 'inv'
  
      setMatrix <- function(y) {        ## define the method named 'setMatrix'
      
        x <<- y
      
      inv <<- NULL
  }
      getMatrix <- function() x        ##define the get fucntion - returns value of the matrix argument
  
      setCache <- function(inverse) inv <<- inverse
  
  ## define the method named 'getCache'
  ## that will return the cached inverse of 'x'
  
      getCache <- function() inv
  
  ## list the names of all method that will be known.
  ## for simplicity, I chose the same name as the methods used above
      
      list(setMatrix = setMatrix, getMatrix = getMatrix, setCache = setCache, getCache = getCache)
}
      cacheSolve <- function(x, ...) {
  
  ## cacheSolve
  ## return the inverse of a given matrix utilizing the cache
  ## check the content of cache matrix
  
     inv <- x$getCache()
  
  ## if the content is not null then: return the result 
  
     if (!is.null(inv)) {
         message("getting cached data...")
         return(inv)
  }
  
  # if the content is empty then: 
  # get the matrix, create, set, update and return the cache matrix
     else {
       
        data <- x$getMatrix()
        invmatrix <- solve(data, ...)
        x$setCache(inv)
        return(inv)
  }
}