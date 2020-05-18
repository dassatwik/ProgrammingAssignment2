## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        m<-NULL
        set_matrix<-function(y=matrix){
                x<<-y
                m<<-NULL
        }
        
        get_matrix<-function() x
        
        set_inverse<-function(z=matrix) {
                m<<-solve(z)
        }        
        
        get_inverse<-function() m
        
        list(set_matrix=set_matrix, get_matrix=get_matrix, set_inverse=set_inverse, get_inverse=get_inverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m<-x$get_inverse()
        if(!is.null(m)){
                message("getting cache data")
                return(m)
        }
        
        matrix_data<-x$get_matrix
        m<-solve(matrix_data)
        x$set_inverse(m)
        m
        
}
