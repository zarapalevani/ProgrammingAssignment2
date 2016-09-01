> ## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
> ## make matrix assign to variable x, and initialize m to NULL
> 
> makeCacheMatrix <- function(x = matrix()) {
+     m <- NULL
+     set <- function(y) {            ## if user want to reset matrix 
+         x <<- y                     ## reassign "new" matrix to x 
+         m <<- NULL                  ## reinitialize m to NULL
+     }
+     get <- function() x
+     setInvmatrix <- function(InvMatrix) m <<- InvMatrix
+     getInvmatrix <- function() m
+     list(set = set, get = get,
+          setInvmatrix = setInvmatrix,
+          getInvmatrix = getInvmatrix)
+     
+ }
> 
> ## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
> ## Return a matrix that is the inverse of 'x'
> 
> cacheSolve <- function(x, ...) {
+     
+     m <- x$getInvmatrix()              
+     if(!is.null(m)) {           ## if user had calculated the same matrix before
+         message("getting cached data")  
+         return(m)               ## return old result(m) directly 
+     }
+     data <- x$get()             ## otherwise, get the uncalculated matrix
+     m <- solve(data, ...)       ## calculate the inverse matrix
+     x$setInvmatrix(m)           ## reassign inverse matrix 
+     m                           ## print the inverse matrix 
+ }
