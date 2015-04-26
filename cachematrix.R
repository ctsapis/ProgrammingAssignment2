## Caching the Inverse of a Matrix (assuming that the matrix supplied is always invertible)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
          a<-NULL
          set<-function(y){
                    x<<-y
                    a<<-NULL
          }
          get<-function() x
          setmatrix<-function(solve) 
                    a<<- solve
          getmatrix<-function() a
          list(set=set, 
               get=get,
               setmatrix=setmatrix,
               getmatrix=getmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x=matrix(), ...) {
          a<-x$getmatrix()
          if(!is.null(a)){
                    message("getting cached data")
                    return(a)
          }
          matrix<-x$get()
          a<-solve(matrix, ...)
          x$setmatrix(a)
          a
}