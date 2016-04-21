## Assignment 2: computing the inverse of the special matrix by
## "makeCacheMatrix" and "CacheSolve"

## The role of this function is to create a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inve <<- inverse
  getInverse <- function() inve
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inve <- x$getInverse()
  if (!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setInverse(inve)
  inve
}


##Test
my_matrix <- makeCacheMatrix(matrix(c(3,9,12,23,45,67,-1,1,23), 3, 3))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()


##Solution
# > my_matrix$get()
# [,1] [,2] [,3]
# [1,]    3   23   -1
# [2,]    9   45    1
# [3,]   12   67   23
# > my_matrix$getInverse()
# NULL
# > cacheSolve(my_matrix)
# [,1]        [,2]        [,3]
# [1,] -0.58880779  0.36253041 -0.04136253
# [2,]  0.11861314 -0.04927007  0.00729927
# [3,] -0.03832117 -0.04562044  0.04379562
# > cacheSolve(my_matrix)
# getting cached data
# [,1]        [,2]        [,3]
# [1,] -0.58880779  0.36253041 -0.04136253
# [2,]  0.11861314 -0.04927007  0.00729927
# [3,] -0.03832117 -0.04562044  0.04379562
# > my_matrix$getInverse()
# [,1]        [,2]        [,3]
# [1,] -0.58880779  0.36253041 -0.04136253
# [2,]  0.11861314 -0.04927007  0.00729927
# [3,] -0.03832117 -0.04562044  0.04379562
