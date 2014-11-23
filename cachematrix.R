## The two functions makeCacheMatrix () and cacheSolve () 
## can cache the inverse of a matrix if the respective matrix is invertible.
## 

## makeCacheMatrix () is a function that creates a 
## special "matrix" object that can cache its inverse.
## Therefore, an invertible matrix has to be passed 
## as an argument to the function. This function creates four other functions and
## stores the initial matrix to pass it to the next function (cacheSolve()).
## The set function allows to assign new values to the objected created by makeCacheMatrix ().

makeCacheMatrix <- function(x = matrix()) {
  
    inversion <- NULL
    set <- function (y) {
      x <- y
      inversion <<- NULL
      }

    get <- function () {x}
    setinversion <- function (solve) {inversion <<- solve}
    getinversion <- function () {inversion}
    
    list (set = set,
          get = get,
          setinversion = setinversion,
          getinversion = getinversion)
}


## 
## cacheSolve () checks whether the fetched value for 'inversion' 
## is not NULL, if so the data are taken from the catch.
## Otherwise the matrix will be taken in with x$get() and afterwards inverted
## with x$setinversion(inversion).

cacheSolve <- function(x, ...) {
    inversion <- x$getinversion ()
    if (!is.null (inversion)){
      
      message ("getting catched data")
      return (inversion)
    }
    
    data <- x$get()
    inversion <- solve (data, ...)
    x$setinversion(inversion)
    inversion
        ## Return a matrix that is the inverse of 'x'
}
