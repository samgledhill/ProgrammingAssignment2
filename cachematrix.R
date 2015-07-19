## This project contains two functions.
## The first function creates a invertable matrix and sets it to a variable in the parent scope
## The second function assesses to see if the cached matrix is null and if so, inverts it. otherwise it leaves it as is.


## First construct a function which returns 4 functions:
#     1. Set a matrix
#     2. Get a matrix
#     3. Set the inverse
#     4. Get the inverse

makeCacheMatrix <- function(x = matrix()) {
      ## Start with a NULL in the cache
      m <- NULL
      
      ## Define the function to set the matrix value
      set <- function(y) {
            x <<- y
            m <<- NULL  ## Clear the cache once the matrix is set
      }
      
      ## Define the function to get the matrix out of cache
      get <- function() {
            ## Simply return x
            x
      } 
      
      ## Define the function to set the inverse calculation
      setinverse <- function(solve) {
            ## Set cache equal to value of the solve function
            m <<- solve
      } 
      
      ## Define the function to get the inverse from the cache
      getinverse <- function() {
            ## Retrieve m (the cache)
            m
      }
      
      ## Return a list of names of the functions in this cache
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Second, construct a function which determines if the matrix holding variable, m, is empty.  
#     If it is empty, then use solve() to determine and store the inverse
#     If it is not empty (ie - has a value) then pull that value from the cache

cacheSolve <- function(x, ...) {
        ## Get the inverse if it exists from the previous list of functions
      m <- x$getinverse()
      
      ## Check to see if the cache(m) has anything in it
      if(!is.null(m)) {
            ## If not empty, return a message and retrieve the value
            message("Getting Cached data...")
            return(m)
      }
      
      ## If empy cache, get the matrix out of cache
      data <- x$get()
      m <- solve(data)
      x$setinverse(m)
      
      ## Return the cache
      m
}