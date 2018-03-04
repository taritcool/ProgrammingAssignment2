## Below are two functions that are used to create a special object that stores a matrix 
## and cache's its inverse

## The first function, makeVector creates a special "vector", 
## which is really a list containing a function to
## set the value of the vector
## set the value of the vector
## set the value of the Inverse of Matrix
## get the value of the Inverse of matrix
 
makeCacheMatrix <- function(x = matrix()) 
{
  m<-NULL
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setSolve<-function(Solve) m<<-Solve
  getSolve<-function() m
  list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)
}


## The following function calculates the Inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setSolve function

cacheSolve <- function(x, ...) 
{
  m<-x$getSolve()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setSolve(m)
  m
  ## Return a matrix that is the inverse of 'x'        
}