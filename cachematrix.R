## The makeCacheMatrix and cacheSolve functions work together utilizing Lexical Scoping 
## to efficiently provide access to resource intensive inverse matrix calculation results.

## `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.
## The value of this function is to define the "matrix" object and keep it in cached memory
## Example to call function: 
  ## >  m <- matrix object
  ## >  mymatrix <- makeCacheMatrix(m)

makeCacheMatrix <- function(x = matrix()) {
    
  ## upon initialization x is a assigned as a function input variable, mymatrix 
  ## x_inv = x inverse is initialized and set to NULL to ensure that the previous variable assignment is cleared 
  
    x_inv <- NULL

 
  ## sets a new matrix object and clears previous inverse matrix, example: mymatrix$set(new_matrix)
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    
  ## gets the existing matrix that was defined in the initial mymatrix <- makeCacheMatrix(m) call or mymatrix$set() call
  ## also referenced by the cacheSolve function
    get <- function() x
    
  ## used by cacheSovle function to save the inverse matrix to cache
    setinv <- function(solve) x_inv <<- solve
    
  ## used by cacheSolve function to determine if the inverse matrix needs to be calculated, 
  ##can be referenced by mymatrix$getinv()
    getinv <- function() x_inv
  
  ## allows the set, get, setinv, and getinv functions to be referenced by name using the $ subsetting operator   
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then `cacheSolve` will retrieve the inverse from the cache.
## Example to call function:
  ## >  cacheSolve(mymatrix)
cacheSolve <- function(x, ...) {
    
    x_inv <- x$getinv()             ## retrieve inverse matrix from makeCacheMatrix
    if(!is.null(x_inv)) {           ## check if an inverse matrix is in cache
      message("getting cached data")
      return(x_inv)                 ## return inverse matrix from cache if available
    }
    data <- x$get()                 ## retrieve previously provided matrix from makeCacheMatrix 
    x_inv <- solve(data, ...)       ## Calculate a matrix that is the inverse of 'x'
    x$setinv(x_inv)                 ## assign inverse matrix to makeCacheMatrix
    x_inv                           ## return inverse matrix if it needed to be calculated
}
