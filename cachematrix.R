##Coursera R Programming Course
##Programming Assignment 2: Lexical Scoping
##By Raul Goycoolea
##
## Write the following functions:
##  
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## 
## cacheSolve: This function computes the inverse of the special "matrix" returned bymakeCacheMatrix above
##
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## *** Function makeCacheMatrix 
##
## This is the function definition to calculates a return list containing the functionality over a matrix
##
## The parameter imput must be a matrix 
##
## ******

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  
  ## *** set function 
  ##
  ## The objective of the makeCacheMatrix function is:
  ##
  ## 1.- To create and initialize a matrix named "x" with the values stored in entry parameter of the function.
  ## 2.- To create and inicializate a variable "m" to NULL.  
  ##
  ## 
  ## ****** 
  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  ## *** get function 
  ##
  ## The objective is to get the value of the matrix "x"
  ##
  ## ******
  
  
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  
  ## *** Getting the Inverse Matrix result if exists in memory
  ##
  ## The "m" matrix  only exists if the calculation of the inverse matrix has been executed befores 
  ## or if not the returned value will be NULL
  ## ******
  
  
  getmatrix<-function() m
  
  ## *** list the function definitions
  ##
  ## Here we list the contents of the list which will be returned with the execution of function "makeCacheMatrix"
  ## The returned list contains the 4 parameters with the functionality of the functions as has been defined
  ## 
  ## The benefit is doing in this way is to be able to execute a function repeatedly with out computing again each time is required 
  ##
  ## ******
  
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## *** cacheSolve Function
##
## This funcion calculate and returns a matrix inverse. If the "original" matrix doesn't has an inverse the funcion
## returns an error.
##
## The cacheSolve is set to do:
##
##  The input parameter is the list of functions described on top of the program (makeCacheMatrix)
##
##  If the inverse of the "original matrix" was already calculated, the function gets the inverse matrix which
##     is stored in (in memory). So, no extra processes and memory will be necessary to obtain the matrix inverse again
##
##  If the inverse of the "original matrix" isn't already calculated, the function will calculate it for the
##     first time and will store it in two variables, one stored in the current environment and other stored
##     in another environment (the last one will be used in future executions of the function if it is requested the 
##     inverse matrix of the same "original matrix")
##
##  Finally, the returned variable is the inverse matrix
##
## ******

cacheSolve <- function(x=matrix(), ...) {
  
  ## With this List's function "getmatrix" to get the inverse matrix which is located in the "other environment"
  ## and store it in the variable "m" in the current environment.
  
  m<-x$getmatrix()
  
  ## If the inverse matrix in "m" is not null, it means that the inverse matrix of the "original matrix" were already
  ## calculated and it was stored in the "other environment".
  ## Then the returned parameter is the already calculated inverse matrix and also a message to alert the user that
  ## this inverse matrix was already cached. If it's take case the function ends with the return(m)
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## Or if the inverse matrix in "m" is null, it means that the inverse matrix isn't still calculated,
  ## so the "original matrix" has to been obtained using the List's fuction "get" and be stored in the variable "matrix" in the
  ## environment.
  
  matrix<-x$get()
  
  ## Now, the inverse matrix is calculated using the function "solve" and it stores the result in 
  ## the variable "m" in the current environment.
  
  m<-solve(matrix, ...)
  
  ## Once the inverse matrix is stored in "m", the List's function "setmatrix" is called and 
  ## the inverse matrix is stored in the "other environment", so it could be used in the future.
  
  x$setmatrix(m)
  m
  ## Return a matrix that is the inverse of 'x' from the "m" variable
}

## Thanks for reviewing my code, hope you enjoy it.
## Regards