Coursera R Programming Course
Programming Assignment 2: Lexical Scoping
By Raul Goycoolea
 
Write the following functions:
  
makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
cacheSolve: This function computes the inverse of the special "matrix" returned by 
makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'


