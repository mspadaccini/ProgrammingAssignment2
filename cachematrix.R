############################################################
#Cache Inverse Matrix Function cacheSolve(cacheMatrixObj)

#Purpose: To compute the inverse of a matrix.  When possible
#use prior computed inverse matrix stored in parent object to
#save computing power. Assumes all matrices are invertable.

#Modified By: Michael Spadaccini
#Modified Date: 10/16/2016

############################################################ 

#Establish an object that stores a matrix in the parent
makeCacheMatrix <- function(x = numeric()){
      s <- NULL
      
      #Setter Function
      set <- function(y){
            x <<- y
            s <<- NULL
      }
      
      #Getter Function
      get <- function() x
      
      #Methods Calls
      setInverse <- function(solve) s <<- solve
      getInverse <- function() s
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#Returns the inverse of a matrix.  Uses cache if already computed, otherwise
#compute the matrix inverse using the solve function
cacheSolve <- function(x, ...){
      s <- x$getInverse()
      
      #If cache is found, return it and notify user
      if(!is.null(s)){
            message("getting cached data")
            return(s)
      }
      
      #Else compute the inverse
      data <- x$get()
      s <- solve(data, ...)
      x$setInverse(s)
      s
}
