## The two functions below are used to cache the inverse of a given square
## matrix, therefore avoiding the inverse to be computed everytime.

## The first function (makeCacheMatrix) is designed in a way that the value
## of matrix x and its inverse inv are stored in the Envirmonment created within
## the function. makeCacheMatrix returns a list of functions that allows us to 
## interact with its environment.
## The second function (cacheSolve) either calculates the inverse of the matrix x (created
## with makeCacheMatrix), or recovers the cached inverse if it has already been 
## calculated.



## makeCacheMatrix returns a list of functions (set, get, setinv, getinv) which
## allows us to interact with the own function's environment by setting (or getting)
## the value of a square matrix x, and setting (or getting) its inverse inv. Both
## x and inv are stored within makeCacheMatrix's environment.

makeCacheMatrix <- function(x = matrix()) {

  inv<-NULL # initializes the value of the inverse
  
  # set is a function that allows to set a value for matrix x. Each time it is used
  # the value of the inverse inv in the enclosin environment must be set to NULL
  # because as the original matrix was changed, the old inverse must be excluded from
  # cache
  
  set<-function(y){
    x<<-y #defines the new matrix
    inv<<-NULL #resets the cached inverse
  }
  
  # get prints the value of x
  
  get<-function() x
  
  # setinv sets a value for the inverse inv
  
  setinv<-function(xinverse){inv<<-xinverse}
  
  # getinv prints the inverse inv
  
  getinv<-function() inv
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## CacheSolve gets the matrix generated with makeCacheMatrix and returns its cached
## inverse (if it has already been computed). Otherwise it computes the inverse and
## caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # returns the chached inverse if its value exists (isn't NULL)
  inv<-x$getinv()
  if (!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  
  # computes inverse otherwise, using function solve, caches and prints it
  xmatrix<-x$get()
  inv<-solve(xmatrix, ...)
  x$setinv(inv)
  inv
}
