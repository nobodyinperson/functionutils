#### Integration ####

#' discrete integration of series or a function
#' @description Numeric discrete integration of series or function \code{y} over variable \code{x}.
#' @param y The values to integrate. Either a numeric vector of function values 
#'  corresponding to elements in \code{x} or a function that takes a single 
#'  argument \code{x} and returns the corresponding value \code{y}.\cr
#'  For example: \code{y = function(x) x ^ 2}.
#' @param x numeric vector of \code{y}-value-positions. Can be left unspecified
#'  if \code{from}, \code{to} and \code{n} are given.
#' @param from lower bound of integration, will be used as smallest \code{x}. Unused if \code{x} is specified.
#' @param to upper bound of integration, will be used as highest \code{x}. Unused if \code{x} is specified.
#' @param n integer. Amount of steps / resolution of integration. Determines the
#'  length of \code{x}. Unused if \code{x} is specified.
#' @param method integration method. Either \code{center},\code{left},\code{right},\code{upper} or \code{lower}.
#' @return numeric value of integration
#' @export
integration = function(y,x = seq(f=from,t=to,len=n),from = min(x),to = max(x),n = length(x), method="center") {
	n <- as.integer(n)
	if( n < 2 ) stop("n must be greater than 1.")
	
	yint = numeric(n)
	if(is.numeric(y)) { # y is not numeric, must be a function!
		if(length(x) != length(y)) stop("Lengths of y and x don't match!")
		yint = y
	} else { # y is numeric
		if(is.function(y) & length(formals(y)) == 1) # Correct function!
			for(i in 1:n) yint[i] = y(x[i]) # Fill integration values with function values
		else
			stop("y must be either a numeric vector or a function with one argument!")
	}
	
	# Everything is fine, now integrate!
	if(method=="center") # Linear interpolation to the center
		sum( (yint[1:(length(yint)-1)] + diff(yint) / 2) * diff(x) )
	else if(method=="left") # Left sum
		sum( yint[1:(length(yint)-1)] * diff(x) )
	else if(method=="right") # Right sum
		sum( yint[2:(length(yint))] * diff(x) )
	else if(method=="lower") # Lower sum
		sum( pmin(yint[1:(length(yint)-1)],yint[2:length(yint)]) * diff(x) )
	else if(method=="upper") # Upper sum
		sum( pmax(yint[1:(length(yint)-1)],yint[2:length(yint)]) * diff(x) )
  else
  	stop("method must be one of center,left,right,upper and lower.")
	
}
