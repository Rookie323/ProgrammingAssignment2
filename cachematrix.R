
##This function gets a square and invertiblbe matrix and puts it into Cache.  

##This function also has 4 other functions:  set, get, setInverse, and getInverse
makeCacheMatrix <- function(x = matrix()) {

        inv <-NULL
  
  set <- function (y) {
    
    x <<- y
    inv <<- NULL
    
  }
  
  get <- function () x
    setInverse <- function (inverse) inv <<- inverse
    getInverse <- function ()  inv
    
    
    list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
  }


## This function either retrieves the inverse in Cache or solves for the matrix and sets the inverse in Cache
## it is awesome the matrix is invertible.  
cacheSolve <- function(x, ...) {
       
  m <- x$getInverse()
  
  if (!is.null(m)) {
    
    message ("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
}
