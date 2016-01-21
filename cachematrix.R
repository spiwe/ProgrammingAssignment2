##The first function, *makeCacheMatrix* stores and caches the Inverse of a Matrix and contains the following list:
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse matrix
##  get the value of the inverse matrix
##NB: The function  *makeCacheMatrix* creates a special "matrix" object that can cache its inverse.

 makeCacheMatrix <- function(x = matrix()) {
    invs <- NULL
    set <- function(y) {
      x <<- y
      invs <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invs <<- inverse
    getInverse <- function() invs
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
   }


## The second function  *cacheSolve* calculates the inverse of the special *matrix* created with *makeCacheMatrix*.
##Firstly, it checks to see if the inverse has already been calculated. 
##If yes it get`s the inverse from the cache and skips the computation.
##Or else, it computes the inverse ofthe matrix and sets the value in the cache via the *setInverseset*function.

cacheSolve <- function(x, ...) {
     invs <- x$getInverse()
     if (!is.null(invs)) {
       message("getting cached data")
       return(invs)
     }
     mtx <- x$get()
     invs <- solve(mtx, ...)
     x$setInverse(invs)
     invs
   }


