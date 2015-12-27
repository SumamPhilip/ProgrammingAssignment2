## The whole idea of the code is to reduce the processing time of execution and reuse the value of objects by appling the concept of lexical scoping 
## The below code calculates the inverse of matrix when called the first time and caches its output. 
## If the same function is called again with same matrix, then instead of calculating the matrix over again , the code just takes the value from the cached obbject.
## This increases efficiency of code by making it more faster.


## The below function takes a matrix as its input. It is a collection of getter and setter function.
## get() : Get the value stored in input matrix 
## set() : Stores value to the matrix and creates an empty inverse matrix to store the inverse of the input matrix. 
## getinverse() : Get the inverse matrix of input matrix
## setinverse() : It sets the input matrix to the inverse matrix variable.  
makeCacheMatrix <- function(x = matrix()) {
	  x.inverse <- NULL
        set <- function(y) {
        	x <<- y
                x.inverse <<- matrix(nrow=0,ncol = 0)
        }
        get <- function() x
        setinverse <- function(inverse) x.inverse <<- inverse
        getinverse <- function() x.inverse
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
##This function takes the output of function - makeCacheMatrix() as its input object. It first gets the value of the matrix stored in it and the value of its inverse as assigned by makeCacheMatrix(). 
## The function first calculates the validity of matric i.e it should be of data type numeric, integer, complex or logical to be able to find inverse
## Then the function checks if the matrix has valid values. (invalid values : NA, NULL,Inf,NaN)
## TThe function then checks if the matrix is invertible sqaure matrix of not.
## In either of the above cases, appropriate message will be printed and the returned inverse matrix would have the value initially assigned by makeCacheMatrix() i.e. NULL.
## On calling the function with a valid matrix, it calcultes the inverse of the matrix,stores that value to the x.inverse and returns its value.
## if the below function is called mutiple times with same matrix value then, instead of again calculating the value of inverse it gets the cached value of inverse for the recurrent matrix.
cacheSolve <- function(x, ...) {
        m<-x$get() ## Get the matrix
        x.inverse <-x$getinverse() # get inverse
        ## check if matrix is valid - numeric,logical,complex,integer
        if(!(class(m[1]) %in% c("numeric","logical", "integer", "complex")))
	{
	        message("The matrix is not of valid data type - numeric | logical | complex | integer")
                message("No inverse can be calculated")
        } else
	{
	        # Check for NA,Inf,Nan,NULL(invalid values) in the matrix
        	if(any(is.na(m))|any(is.null(m))|any(is.infinite(m)))
        	{
                	message("The matrix contains invalid value - NA | NULL | Inf | NaN")
        	 	message("No inverse can be calculated")
        	} else 
        	{
        		## Check for rectangular matrix
		   	## For a non square matrix, number of columns is not equal to number of rows
                	if(dim(m)[1]!=dim(m)[2])
               		{
                        	message("The matrix is a rectangular matrix")
                        	message("No inverse can be calculated")
                	} else
                	{
                		## Check for specific conditions of square matrix                              
                		## Square sigular or non-invertible matrix (determinent is 0)
                		## Square invertible
                		if (class(m[1])=="complex") ## R doesnt support finding determinent of complex number matrix
                		{d=1} else {d=det(m)}       ## assign a non-zero to 'd' if its a complex number matrix else find the determinent
                        	if(d==0)
                        	{ 	## Determinent of a singular or non-invertible matrix is zero, henceits inverse cant be calculated
	                                message("The matrix is a singular or non-invertible square matrix")
        	                        message("No inverse can be calculated")
                	        } else 
                        	{
                                	message("The matrix is a invertible square matrix")
                                	if(length(x.inverse)!=0) 
                                	{
                                        	message("Getting cached data")
                                        	message("Cached Inverse of Matrix")
                                	        return(x.inverse)
                                	}
                                	x.inverse <- solve(m) ## Calculate inverse of matrix
                        		x$setinverse(x.inverse)  ## Updates the inverse in defined envirnmonet
                        	}
                	}
        	}
  	}
    	message("Inverse of Matrix")
    	return(x.inverse)
    	## return a NULL value when the matrix is of invalid data type, has invalid values or is singular or non-invertible.
    	## returns the inverse matrix. When function is called multiple times with same matrix, the function returns the cached matrix.
}
