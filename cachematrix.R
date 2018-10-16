## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

matrices <- list()                                          ## Creating a list to store lists of matrices, containing the original matrix and its inverse at each position

makeCacheMatrix <- function(x = matrix()) {
   d <- dim(x)[1]                                           ## Theoretically the matrix is already tested to be square
   cache_matrix <- matrix(NA,nrow = d,ncol = d)             ## Creating empty matrix for cache
   matrices[[length(matrices)+1]]<<-list(x, cache_matrix)   ## Adding matrix and cache matrix to the last position
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   height <- dim(x)[1]
   width <- dim(x)[2]
   inverse_matrix <- matrix(NA,height,width)                ## Creating empty inverse matrix to be returned
   if(height!=width){
      print("The matrix should be square!")                 ## Testing if matrix is square
   }
   else{
      matlistsize <- length(matrices)                       ## Taking the list size
      i <- 1                                                ## Counter
      found <- FALSE                                        ## Flag for matrix already seen before
      while(i<=matlistsize && !found){
         if(identical(matrices[[i]][[1]],x)){               ## Comparing matrix to those already seen before
            found <- TRUE
            inverse_matrix <- matrices[[i]][[2]]
         }
         else{
            i <- i + 1                                      ## If not found yet, increment the counter
         }
      }
      if(!found){                                           ## If the inverse was not found, calculate it!
         makeCacheMatrix(x)                                 ## Store in the list of matrices
         inverse_matrix <- solve(x)                         ## Calculate the inverse
         matrices[[length(matrices)]][[2]]<<-inverse_matrix ## And store the inverse
      }
   }
   inverse_matrix
}