##This script consists of two functions: (1) makeCacheMatrix, a special "matrix" will be created; and (2) cacheSolve, where the inverse of the Matrix will be cached. 
##As an example,create an object "AMat" by the command AMat <- makeCacheMatrix(A), where A is an inversible matrix.
##To retrieve A, use the command AMat$get()
##To calculate and cache the inverse of A, use the command cachemean(AMat)
##To retrieve the inverse of A, use the command AMat$getinverse()
##The inverse of A is cached so recaculation is not required.
##If another object, e.g. BMat <- makeCacheMatrix(B), is created, then NULL is assiged to object inv.
##The inverse can be calculated with the command cachemean(BMat), and so on.

makeCacheMatrix <- function(x = matrix()) {
        
##Initialize the objects inv
	inv <- NULL

##Assign the matrix input to object x, and NULL to object inv
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
##Return the matrix object x
	get <- function() x

##Assign the inverse value calculated by the cacheSolve function to the object inv
        setinverse <- function(inverse) inv <<- inverse

##Returns the object inv as output
        getinverse <- function() inv
 
## Return the functions "set","get","setinverse","getinverse" as the output
       list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##This function solve the matrix created in the makeCacheMatrix function and return the inverse

cacheSolve <- function(x, ...) {

##Initialize the object inv with x$getinverse()
       inv <- x$getinverse()
 
##Determine whether the matrix is already solved and the inverse is cached
       if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
##Assign the object created with the makeCacheMatrix function to the object matrix
        matrix <- x$get()

##solve for the inverse of the object matrix and assign the result to the object inv
        inv <- solve(matrix, ...)

##Assign the calculated inverse value to the object inv in the makeCacheMatrix function
        x$setinverse(inv)
        inv
}
