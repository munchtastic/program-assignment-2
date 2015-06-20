## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Function takes a matrix as an input and returns a "list" of objects containing the inputted matrix, 
#functions for setting and retrieving inverse, resetting the matrix, and a matrix where the values are inversed.

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) i<<- solve
    getinverse<-function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
#Function takes the special "list" of object outputted by makeCacheMatrix as input. It checks if inverse (i) has a value,
#, indication of inverse already calculated, and if it's empty, it calculates the inverse and enters the results into cache.
#Finally, it returns the calculated matrix containing the inversed values.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
