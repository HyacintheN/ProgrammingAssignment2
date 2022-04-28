## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {#makeCacheMatrix function create a matrix tha cashes its inverse
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#testing
a= matrix(c(1,2,4,3,1,5,7,5,7,3,5,6,7,8,9,6), nrow = 4, ncol = 4, byrow = TRUE)
a
solve(a)
a1 <- makeCacheMatrix(a)


## Write a short comment describing this function

cacheSolve <- function(x, ...) {#cacheSolve function computes cashed inverse of matrix makeCacheMatrix
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
}
#testing
a= matrix(c(1,2,4,3,1,5,7,5,7,3,5,6,7,8,9,6), nrow = 4, ncol = 4, byrow = TRUE)
a
solve(a)
a2 <- cacheSolve (a1)
a2       
#solve(a)==cacheSolve (a1)
