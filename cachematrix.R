## Programming assignment 2: define a matrix that can cache its inverse matrix via makeCacheMatrix/cacheSolve functions

## Create a matrix capable of holding cache of its inverse matrix, which can be obtained with cacheSolve(x)
## arguments:
##  x - actual matrix. defaults to empty matrix
makeCacheMatrix <- function(x = matrix()) {
  cachedSolution <- NULL
  
  set <- function (newMatrix) {
    x <<- newMatrix
    cachedSolution <<- null
  }

  get <- function () x
  
  
  getCachedSolution <- function () cachedSolution
  setCachedSolution <- function (solution) cachedSolution <<- solution
  

  list(set = set, get = get,
       getCachedSolution = getCachedSolution,
       setCachedSolution = setCachedSolution)
}


## Solve a matrix x, where x is a result of makeCacheMatrix(actualMatrix) call
## will call solve(actualMatrix)   only in case it was not called before
## arguments:
## x   - matrix from makeCacheMatrix
## ... - passed to solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # cached?
  result <- x$getCachedSolution();
  if(!is.null(result)) {
    # yes. done.
    message("getting cached")
    result
  }
  # not cached. compute
  result = solve(x$get(), ...)
  x$setCachedSolution(result)
  #return result
  result
}

########
# Test results:
## Construct input
# m <- rbind(c(1, -1/4), c(-1/4, 1))
# > m
# [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
#
## solve once. notice no "getting cached"
# mCached <- makeCacheMatrix(m)
# > cacheSolve(mCached)
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
#
## solve once more notice no "getting cached"
# > cacheSolve(mCached)
# getting cached
# [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
