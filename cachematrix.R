#########################################################################
#	File	:	cachematrix.R											#
#	Author	:	Edlin													#
#	Date	:	Aug. 18, 2014											#
#########################################################################

#	There are two functions below. They can be used to calculate the 
#	invert matrix of an given matrix. If the invert matrix has been
#	calculated first, the program can directly give you back the answer
#	without calculate the invert matrix again.



#	Function "makeCacheMatrix": Create a special "matrix", which is 
#	really a list containing a function to
#		1. get the value of the matrix
#		2. get the invert matrix of the matrix
#		3. set the invert matrix of the matrix

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	get <- function()			x
	getInv <- function()		inv
	setInv <- function(invert)	inv <<- invert
	list(get=get, getInv=getInv, setInv=setInv)

}


#	Function "cacheSolve": Calculate the invert matrix of the special
#	"matrix" created with the above function. It will check to see if
#	the invert matrix has already been calculated. If so, it gets the
#	invert matrix from the cache and skips the computation. Otherwise,
#	it calculates the invert matrix of the matrix and sets the value of
#	the invert matrix in the cache via the setInv function.

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
	if(!is.null(inv)){
		message("cache invert matrix")
		inv
	}
	else{
		data <- x$get()
		inv <- solve(data)
		x$setInv(inv)
		inv
	}
}
