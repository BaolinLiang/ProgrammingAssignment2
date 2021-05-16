##  These functions cache the inverse of a matrix

## The makeCacheMatrix function is for building a special object consist of 
##several functions around a given matrix. 

makeCacheMatrix <- function(x = matrix()) {set<-function(y){
                x<<-y 
                m<<-NULL
        }
        get <-function() x #
        setinverse<-function(inverse) m<<-inverse
getinverse<-function() m 
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above or 
##retrieve the inverse from the cache

cacheSolve <- function(x, ...) { 
        m<-x$getinverse() 
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-mean(data, ...) 
        x$setmean(m)
        m        ## Return a matrix that is the inverse of 'x'
}
