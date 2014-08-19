## These two functions work to take a matrix, find its inverse, and cache the inverse for later use

## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        s<-NULL ##Initializes empty s variable for solve result
        set<-function(y){
                x<<-y ##redefines x to be equal to y
                s<<-NULL 
        }
        get<-function(){
                x ##get is a function that gives the matrix "x"
        }
        setsolve<-function(solve){
                s<<-solve ##called by cacheSolve function to store value of s
        }
        getsolve<-function(){##called by cacheSolve, returns cached value of s
                s
        }
        list(set=set, get=get, 
             setsolve=setsolve, 
             getsolve=getsolve) ##generates a list of functions within 
                                ##makeCacheMatrix so they can be accessed 
                                ##using $ operator in cacheSolve
}


## This function actually creates and stores the inverse of the created matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s<-x$getsolve() ##accesses the getsolve function in makeCacheMatrix 
        if(!is.null(s)){ ##if value of getsolve function is not NULL
                message("getting cached data") ##message is displayed
                return(s) ##and inverse of matrix is returned
        }
        data<-x$get() ##if s is NULL, then set data equal to matrix x
        s<-solve(data, ...) ##solve for the inverse of the matrix
        x$setsolve(s) ##store the calculated inverse in makeCacheMatrix
        s ##return the calculated inverse
}
