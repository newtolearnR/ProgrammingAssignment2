 

## This function takes input as a matrix
## the matrix has to be a square and  invertible matrix
## In case the matrix is not invertible it throws an error(the same is not handled in this function)

makeCacheMatrix<-function(x=matrix()){

         ## sets x equal to an empty matrix

         inverse<-NULL  
         set<-function(t){
          x<<-t

         ## set function assigns the argument to x

         inverse<<-NULL}

         ## after the set function is called,inverse is set to NULL

         get<-function() x

 	 ## get function returns the matrix

         setinverse<-function(mat)
          inverse<<-mat

         ## setInverse overrides the previous value of inverse
         ## and assigns inverse of the matrix to variable "inverse"
        
         getinverse <- function() inverse

         ## returns inverse of the matrix passed

         list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

         ## creates a list of the functions
}


## This function computes the inverse of the matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cachesolve
## retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()

        ## gets the most recent value for inverse of a matrix and assigns to variable inverse

        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)

        ## If the value of inverse is NOT null
        ## then cacheSolve returns inverse value of the matrix from the cache

        }

        ## If the value of inverse is NULL, then retrive matrix x 
        ## and calculate the inverse using function solve() 

        message(" not getting from cached data")
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
