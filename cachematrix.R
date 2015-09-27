## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix stores a matrix X in memory
## cacheSolve shows the inverse of a matrix if is in memory or computes the inverse and then shows the inverse

## Write a short comment describing this function
## makeCacheMatrix uses scoping rules and stores matrices in memory

## The following functions calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and some associated sub-functions/methods
  
  ## define the cache inverse 
  
  inverse <- NULL
  set <- function(Y){
    X <<- Y ## assign the input matrix y to the variable x in the
    ## parent environment
    inverse <<- NULL ## re-initialize m in the parent environment to null
  }
  get <- function() X ## return the matrix x
  setinverse <- function(Inverse) inverse <<- Inverse ## set the cache inverse equal
  ## to the Inverse of the matrix x
  getinverse <- function() inverse ## return the cached inverse of x
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function
## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal descomposition
## note: this function will try to load corpcor library and if it's not installed will try to install the library

## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  if(require("corpcor")){
    print("corpcor is loaded correctly")
  } else {
    print("trying to install corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor installed and loaded")
    } else {
      stop("could not install corpcor")
    }
  }
  inverse <- X$getinverse()
  if(!is.null(inverse)){
    message("matrix is in memory")
    return(inverse)
  }
  message("inverse is not in memory so the inverse (if exist) is gonna be computed")
  data <- X$get()
  inverse <- pseudoinverse(data, ...)
  X$setinverse(inverse)
  inverse
}

#Experiment to try if it works
#square matrix
X <- matrix(rpois(25,3), nrow = 5)
cX <- makeCacheMatrix(X)
cX$get()
cacheSolve(cX)
cacheSolve(cX)
invX <- cacheSolve(cX)

#Experiment to try if it works
#rectangular matrix rows > cols
Y <- matrix(rpois(20,2), nrow = 5, ncol = 4)
cY <- makeCacheMatrix(Y)
cY$get()
cacheSolve(cY)
cacheSolve(cY)
invY <- cacheSolve(cY)

#Experiment to try if it works
#rectangular matrix rows < cols
Z <- matrix(rpois(20,1), nrow = 4, ncol = 5)
cZ <- makeCacheMatrix(Z)
cZ$get()
cacheSolve(cZ)
cacheSolve(cZ)
invZ <- cacheSolve(cZ)

#Experiment to try if it works
#multiplication must return identity or closer
invX %*% X 
X %*% invX
invY %*% Y 
Z %*% invZ