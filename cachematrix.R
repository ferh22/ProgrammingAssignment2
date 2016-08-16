##ProgrammingAssignment2

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are two functions that are used to create an object that stores a matrix and caches its inverse.

## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
+     m <- NULL
+     set <- function (y) {
+       x <<- y
+       m <<- NULL
+     }
+     get <-function() x
+     setmatrix <- function(solve) m<<- solve
+     getmatrix <- function() m
+     list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
+ }

## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

 cacheSolve <- function(x, ...) {
+   m <- x$getmatrix()
+   if(!is.null(m)){
+     message("getting cached data")
+     return(m)
+   }
+   data <- x$get()
+   m <- solve(data,...)
+   x$setmatrix(m)
+   m
+   ## Return a matrix that is the inverse of 'x'
+ }

##Example

matrix <- matrix(1:4,2)
cmatrix <- makeCacheMatrix(matrix)
cacheSolve(cmatrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

