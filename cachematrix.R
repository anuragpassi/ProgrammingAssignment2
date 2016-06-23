## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will set the inverse of the matrix M in inv_mat object.

makeCacheMatrix <- function(M = matrix()) {
	inv_mat <- NULL
        set <- function(Y) {
                M <<- Y
                inv_mat <<- NULL
        }
        get <- function() M
        set_inv <- function(inv) inv_mat <<- inv
        get_inv <- function() inv_mat
        list(set = set_inv, get = get,
             set_inv = set_inv,
             get_inv = get_inv)

}


## Write a short comment describing this function
## Here we will use the solve() to find the inverse of the matrix
## assumed to be invertibleby default.

cacheSolve <- function(M, ...) {
        ## Return a matrix that is the inverse of 'M'
	inv_mat<-M$get_inv()
	if(!is.null(inv_mat)){
		message("getting cached matrix inverse")
		return(inv_mat)
	}
	mat<-M$get()
	inv_mat<-solve(mat)
	M$set_inv<-inv_mat
	inv_mat
}
