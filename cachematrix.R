## There are two functions listed below (a) makeCacheMatrix (b) cacheSolve
## Both these functions are used to find inverse of invertible matrix and put output in a cache 

## This function returns a list of objects containing matrixes and put them in a cache

makeCacheMatrix <- function(x = matrix())
{
	m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
	checkIfNotInvertible <- function(X)
	{
		out <- tryCatch(solve(X) %*% X, error = function(e) e)
		any(class(out) == "error")
	}
	
    get <- function() x
    setinverse <- function(inverse) {m <<- inverse}
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse,
		 isnotinvertible = checkIfNotInvertible)
}



## This function returns inverse of invertible matrixes from cache barring the first call

cacheSolve <- function(x,...)
{
	m <- x$getinverse()	
	if(!is.null(m)) {
			print("getting cached data")
			return(m)
	}
	else
	{
		print("data not cached")
	}
	data <- x$get()	
	if(!x$isnotinvertible(data))
	{		
		m <- solve(data)
		x$setinverse(m)
		m
	}
	else
	{
		print("Given matirx is not invertible")
	}
	
}