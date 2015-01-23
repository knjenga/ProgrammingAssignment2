## 
## The makeCacheMatrix function calcualtes the inverse of a square matrix
## The caluculation is computational expensive, therfore the result is cached 
## Any funtion that requires the inverse matix can retrive it from the cach
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

##
## checks to see if the inverse matix has been calcualted and stored in the cache
## if it has then it retives it from the cache 
## if not then it dose the inverse matix calculation
##
cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## if the calculated inverse of the matix has been calcualated
  ## retive the the it from the cache
    if(!is.null(m)) {
    print("getting cached inverse for matrix")
    return(m)
  }
  ## if the inverse of the matix has NOT been calcualated do the calculation
  print("No cached inverse for the matrix!, perfroming calculation") 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
