## This function generates a list of functions to get/set the matrix we're working on and it's inverse
makeCacheMatrix <- function(x = matrix()) {
    
    #Initialize the inverse matrix to NULL
    inv_matrix <- NULL
    
    #Get function that returns the matrix
    get <- function() x
    
    #Set function that sets the matrix to the new one, so the cached inverse matrix may be wrong (set to NULL)
    set <- function(mat){
        x <<- mat
        inv_matrix <<- NULL
    }
    
    #Get function that retrieves the inverse matrix, or a NULL value in case it hasn't been calculated yet
    getInverse <- function() inv_matrix
    
    #Set the cached inverse matrix to the one given as an argument
    setInverse <- function(i_mat) inv_matrix <<- i_mat
    
    #Returns a list with the 4 functions
    list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


##This function needs a list of functions like the one makeCacheMatrix creates from the matrix given
#It checks if the inverse matrix has been calculated and cached before. 
#In case it has, it returns that inverse matrix. It calculates the inverse of the matrix and caches it, otherwise (when it's a NULL). 
cacheSolve <- function(x, ...) {
    
    #Retrieve the cached matrix
    i_mat <- x$getInverse()
    
    #If it has been calculated before, the inverse matrix is no longer a NULL value, so we can return it
    if(!is.null(i_mat)){
        message("Returning cached inverse matrix")
        return(i_mat)
    }
    
    #This piece of code only runs if the inverse matrix is NULL, so we need to calculate it and cache it
    aux <- solve(x$get(),...)
    x$setInverse(aux)
    
    #Return the inverse matrix
    aux
}
