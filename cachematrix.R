## The pair of functions below cache the inverse of
## a matrix. 

## Creating a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  v <- NULL
  set <- function(y) {
    y <<- x
    v <<- NULL
  }
  get <- function() x
  
  setinvr <- function(solve) v <<- solve
  getinvr <- function() v
  
  list(set = set, get = get, 
       setinvr = setinvr, 
       getinvr = getinvr)

}


## Computing the inverse of the matrix returned by makeCacheMatrix
## Also retrieving the inverse of the matrix if already calculated 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  v <- x$getinvr()
  if(!is.null(v)) {
    message("getting cached inverse")
    return(v)
  }
  
  message("no cached inverse - compute")
  data <- x$get()
  v <- solve(data)
  x$setinvr(v)
  v
}


## Code below is for testing the pair of functions above.
## Run code below AFTER running the functions above. 
## Use matrix(c(1,5,2,2),2,2) as an example. 

x <- makeCacheMatrix(matrix(c(1,5,2,2),2,2))

## no cached inverse - compute
y <- cacheSolve(x)
y 

## getting cached inverse
y <- cacheSolve(x)
y 

