# nia
nia's repository

## This function, makeCacheMatrix creates a special 
##"matrix" that can cache its inverse
##It is really a list containing a function to
##set the value of the matrix, get the value of the matrix
##set the value of the inverse, get the value of the inverse 


makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set<- function(y) {
    x<<- y
    inv <<-NULL
  }
  get <-function() x
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv 
  list(set = set, get = get, setInverse  = setInverse, 
     getInverse = getInverse)}


## The following function, cacheSolve calculates the inverse
##of the special "matrix" created with makeCacheMatrix. 
##However, it first checks to see if the inverse
##has already been calculated. If so, it gets the inverse 
##from the cache and skips the computation. 
##Otherwise, it calculates the inverse and sets the inverse 
##in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)}
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv) 
  inv}

##test function
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)
cacheSolve(my_matrix)

my_matrix$getInverse()


