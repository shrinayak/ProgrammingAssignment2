## makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
		
		Inv <- NULL 
		
		set <- function(y){ 

                x <<- y 
				Inv <<- NULL 
		}
		
			get <- function() x     # get function returns the matrix 

            setInverse <- function(solve) Inv <<- solve 
			
			getInverse <- function() Inv 

		    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) # create list

    }


## cacheSolve function

cacheSolve <- function(x, ...) {
        
		Inv <- x$getInverse()    # Retrives value for the inverse 
		
		   if(!is.null(Inv)){ 

                message("Get Cached Data...") 

                return(Inv) 
            } 
			
		 message("new data") 

        data <- x$get() # retrive matrix x 

        Inv <- solve(data, ...) 

        x$setInverse(Inv) 

        Inv 
		
}
