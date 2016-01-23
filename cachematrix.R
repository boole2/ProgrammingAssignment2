
#Inverse of a Matrix  is computational intensive, caching of the result
#avoid to waste resource if source matrix doesn't change
#example of call
#
#mx <- matrix(c(12,1.2,-4,-6,13,128,0.22,0.45,10),3,3)
#cm <- makeCacheMatrix(mx)
#cacheSolve(cm)
#.....
#cacheSolve(cm)
# mx <- matrix(1:4,2,2)
# cm$set(mx)
#cacheSolve(cm)


#Function makeCacheMatrix create a Special Matrix Object that cache  
#source square matrix (x)  and his inverse (minv). 

## makeCacheMatrix work as a costructor for Special Matrix Object and his Inverse 
# Argument must be a  squered matrix 

makeCacheMatrix <- function(x = matrix()) {
   if(class(x) != "matrix"){
                message("argument not a matrix")
                return()
        }
        else if (nrow(x) != ncol(x)){
                message(" argument not square matrix")
                return()
        }
        
        
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(invmatrix) minv <<- invmatrix
        getinv <- function() minv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


#cacheSolve - receive as argument the special matrix returned by makeCacheMatrix
#and compute the inverse of embedded x matrix, if yet not cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mi <- x$getinv()
        if(!is.null(mi)) {
                message("getting cached inv matrix")
                return(mi)
        }
        message("not yet cached - compute inversal of squared matrix")
        data <- x$get()
        mi <- solve(data)
        x$setinv(mi)
        mi
}
