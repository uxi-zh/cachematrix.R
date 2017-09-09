## Put comments here that give an overall description of what your
## functions do
# caching the inverse of a matrix rather than repeated computation to speed up. 
## Write a short comment describing this function
# makeCacheMatrix functions works to get a list of a functon to 
# set and get the value of the matrix, 
# and set and get the value of its inverse.
# this function will return 
# a list of set, get setuniverse and getuniverse functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
#based on cache_matrix created above
#set a function to get its inverse, if it is inversible.
#Thus, the first is to check cache matrix inversibility
#If inversible, it will return the inverse of the cache
#If not, it will return the inverse of the matrix, 
# and set the values of the inverse in the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}


#######Test#######
#output begin with "###"
#test_cache<-makeCacheMatrix(matrix(2:5,2,2))
#test_cache$get()
###[,1] [,2]
###[1,]    2    4
###[2,]    3    5
#test_cache$getinverse()
###NULL
#cacheSolve(test_cache)->cached_data
####getting cached data
#test_cache$getinverse()
####[,1] [,2]
####[1,] -2.5    2
####[2,]  1.5   -1
