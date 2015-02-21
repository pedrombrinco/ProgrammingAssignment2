#This code implements the caching of the inverse of a square matrix and the retrival of this inverse if it is already on the cache.

#The function makeCacheMatrix creates an object with the functions to create and read both the matrix and its inverse
makeCacheMatrix <- function(x = numeric()) {
        i <- NULL
        
        #Function that attributes a new matrix to x and clears the inverse
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        # Function that reads the matrix
        get <- function() x
        
        #Function that atributes to the inverse its value
        setinv <- function(solve) i <<- solve
        
        #Function that reads the inverse
        getinv <- function() i
        
        #Creates the list, which is the return of the function
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#Funcion that checks if the inverse of the matrix exists or not in cache and computes it (if needed) in order to display it
cacheSolve <- function(x, ...) {
        
        # Atributes to the variable i what is read in the inverse variable
        i <- x$getinv()
        
        #Checks if i is not null. If that is true, displays the inverse and ends the function
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #Reads the matrix
        data <- x$get()
        
        #Computes the inverse
        i <- solve(data, ...)
        
        #Attributes the inverse to its variable
        x$setinv(i)
        
        #Displays the inverse
        i
}
