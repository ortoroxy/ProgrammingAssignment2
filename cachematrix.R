## These functions allows you to store inverted matrixes in cash 
## in order not to compute it every time when it is needed

## 1st function create a "vector" containing functions of setting and getting
## matrix, setting and getting inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) m <<- solve
  
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function allows to take inverted matrix frome cashe if it exists 
## and create if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}


## Short test: create matrix and apply our functions

data <- matrix(rexp(200, rate=.1), ncol=10, nrow=10)
a <- makeCacheMatrix(data)
cacheSolve(a)
