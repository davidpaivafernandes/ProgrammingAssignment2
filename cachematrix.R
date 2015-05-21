## ============================================================================================
## "Constructor" function
##
## One can invoke it passing a squared matrix: var1 <- makeCacheMatrix(rnorm(9), ncol=3)
## Or not: var2 <- makeCacheMatrix()
##
## Returns an "object"; see the last line of code, which is a list.
##
## ============================================================================================

makeCacheMatrix <- function(x = matrix()) {
  
  # "local" inverse matrix
  m <- NULL
  
  # setter
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # getter
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

## ============================================================================================
## Inverse calculation function
## See test() function bellow for usage examples
## ============================================================================================

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  
  mi <- x$getinverse()
  
  if(!is.null(mi)) {
    message("Just getting the inverse cached matrix.")
    return (mi)
  } else {
    message("Calculating inverse for the first time (maybe time consuming operation).")
  }
  
  # getting original matrix
  data <- x$get()
  
  # calculate inverse
  mi <- solve(data)
  
  # setting the inverse on the original object
  x$setinverse(mi)
  
  mi
  
}

## ============================================================================================
## Test function that would perform some tests and provide some usage examples
## ============================================================================================

test <- function() {
  
  # function to test identity of a matrix
  # TODO: shouldn't assume that m is a squared matrix
  isIdentity <- function(m) {
    sum(m - diag(ncol(m)) < 0.00000001) == length(m)
  }
  
  # create a test matrix
  testmatrix = makeCacheMatrix()
  testmatrix$set(matrix(rnorm(25), ncol = 5))
  
  # test the inverse which should be NULL, for now
  if(!is.null(testmatrix$getinverse())) {
    message("ERROR: Something is wrong; the inverse should be NULL.")
  } else {
    message("OK: the inverse is NULL for now!")
  }
  
  # lets calculate the inverse which will calculate the inverse for the first time
  cacheSolve(testmatrix)
  
  # test the inverse which should NOT be NULL
  if(!is.null(testmatrix$getinverse())) {
    message("OK: the inverse was calculated and is not NULL!")
  } else {
    message("ERROR: Something is wrong; the inverse still is NULL.")
  }
  
  # lets just assert that the inverse is correctly calculated
  # multiplyint the inverse by the matrix should returna an indentity matrix
  if(isIdentity(testmatrix$getinverse() %*% testmatrix$get())) {
    message("OK: inverse correctly calculated")
  } else {
    message("ERROR: inverse calculation is somehow WRONG")
  }
  
  # a second try would not calculate the inverse because is not needed.
  cacheSolve(testmatrix)
  
  # lets use SET to set a new matrix which should "reset" the inverse
  testmatrix$set(matrix(rnorm(25), ncol = 5))
  
  # test the inverse which should be NULL, again
  if(!is.null(testmatrix$getinverse())) {
    message("ERROR: Something is wrong; the inverse should be NULL.")
  } else {
    message("OK: the inverse is NULL again!")
  }
  
  # as the matrix was reseted the inverse will be calculated again.
  cacheSolve(testmatrix);
  
  message("End!")
  
}

