## The below functions facililate the retrival of cached data.
## It calcuates the inverse of a matrix and calculates it inverse.
## If the function to calculate inverse is called 'n' number of times to do the same thing for same imput matrix, then these functions help reduce the process time by retriving the data stored in cache. 
## The code uses the concept of lexical scoping.

## This function accepts a matrix as input and has following getter & setter functions
## get() : Get the value stored in matrix
## set() : Sets a value to the matrix
## getinverse() : Gets the inverse of matrix. Initially, it sets the value of inverse matrix to NULL. It remains NULL till a valid inverse is calculated.  
## setinverse() : Stores the inverse of matrix (passed as argument) into the variable x.inverse  

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


## This function print the inverse of a matrix. It first checks for the validity of the inverse can be taken of the matrix. 
## It returns NULL value for matrix with invalid values(NA,NULL,NaN,Inf), rectangular matrix and singular or non-invertible square matrix (inverse can't be calculated for these matrices)
## It returns the inverse matrix for valid numeric matrix when called the first time and returns cached inverse matrix if called more than once for same matrix.  
cacheSolve <- function(x, ...) {
	      m<-f$get() ## Get the matrix
		x.inverse <-f$getinverse() # get inverse of the matrix
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
		 		message("The matrix contains invalid data - NA | NULL | Inf | NaN")
		 		message("No inverse can be calculated")
			}else 
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
					{d=1}else {d=det(m)} 	    ## assign a non-zero value to 'd' if its a complex number matrix else find the determinent
					if(d==0)
					{     ## Determinent of a singular or non-invertible matrix is zero, henceits inverse cant be calculated
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
        					f$setinverse(x.inverse)  ## Updates the inverse in defined envirnmonet
					}
				}
			}
		}
	   message("Inverse of Matrix")
	   return(x.inverse)
}
