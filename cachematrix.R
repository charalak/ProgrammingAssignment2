## The purpose of this function is first to create a matrix with the
## makeCacheMatrix, which is assumed that can be inverse. Then, 
## function cacheSolve solves the given matrix from the previous
## function and iverse it.

## The makeCacheMatrix creates a matrix that is assumed a priori
## to can be inverse. The argument of x or y should be given in
## as the following example: "x <- matrix( c(1,2, 3, 4),nrow=2,ncol=2)" 
## one can set the matrix and its inverse with seMat and setInverseMat
## respectivly. In addition, one can get the matrix and its inverse with 
## geMat and getInverseMat respectivly.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL  ## i.e. the NULL value set initially
                     ## to the inverse matrix before calculated
        
        ## the setmat function is to set a matrix different than
        ## x matrix when we want it. The y matrix is then assigned
        ## to the x value.
        setMat <- function(y){
                x <<- y 
                inv <<- NULL
        }
        
        ## getmat function returns the matrix
        getMat <- function() x
        
        ## setInverseMat function sets an inverse matrix
        setInverseMat <- function(inverse) inv <<- inverse
        
        ## getInverseMat function gets the inverse matrix
        getInverseMat <- function() inv    
        
        ## next, we create a list of the set or get matrix
        ## in order to can call them as the following example:
        ## foo <- makeCacheMatrix( "my_matrix")
        ## another_matrix <- foo$setmat("another matrix")
        ## the_matrix_is  <- foo$getmat() 
list(setMat = setMat, getMat=getMat, setInverseMat=setInverseMat, 
     getInverseMat=getInverseMat)
}


## The cacheSolve function checks if the inverse matirx is calculated
## If yes then it prints out that. If not it calculates it

cacheSolve <- function(x, ...) {
        ## We retrieve the inverse matrix if exists from the 
        ## function makeCacheMatrix 
        inv <- x$getInverseMat()
        ## We check if indeed the inverse matrix exists
        if(!is.null(inv)){
                message("getting cache inverse matrix")
                return(inv)
        }
        ## if there is not an inverse matrix we retreive the matrix
        ## from makeCacheMatrix function using the following
        mat_data  <- x$getMat()
        ## we used the retrrieved data to calculate the inverse matrix
        inv       <- solve(mat_data)
        ## the calculated inverse matrix is then set into the x via:
        x$setInverseMat(inv)
        ## display on the monitor the inverse matrix
        inv
}
