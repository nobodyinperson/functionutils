#' Substitute parameters in a function with values
#' @description Replace variables in \code{func} with values given in \code{params}. Remove argument names from \code{func} if necessary.
#' @param func a function or expression
#' @param params a list of the form \code{list(param1=value1,param2=value2,...)} with values
#'  to substitute the parameters. Defaults to the variable list in the current environment.\cr
#'  \strong{Hint:} the values may also be \code{\link{quote()}}ed R expressions. See examples.
#' @details 
#'  This is a convenient wrapper around the base function \code{\link{substitute}} extended
#'  by the possibility to substitute values in a function, possibly removing
#'  arguments of the function according to which variables are to be replaced with values.
#' @examples
#' # a simple function
#' f <- function( a, b, c ) a + b + c
#' 
#' substitute.parameters( f, list( a = 1 ) )
#' # a is replaced by 1, the function looses the argument a
#' # function (b, c) 
#' # 1 + b + c
#' 
#' substitute.parameters( f, list( a = quote(b) ))
#' # a is replaced with b, the function looses the argument a
#' # function (b, c) 
#' # b + b + c
#' 
#' a = 10 # Set global value for a
#' substitute.parameters( f ) # no params argument given
#' # By default, params are taken from the environment of f
#' # function (b, c) 
#' # 10 + b + c
#' @export
substitute.parameters = function(func,params=as.list(environment(func))) {
	FUNC = func # Die übergebene Funktion
	ENV = environment(func) # Environment der übergebenen Funktion
	ARGS = params # Die übergebene Parameterliste
	
	# Wenn keine Liste als param übergeben wurde
	if(!is.list(ARGS)) stop("'params' must be a named list!")
	
	# In den Parameters auch Funktionen zulassen, dort nur die bodys nehmen
	# Irgendwie unnötig...
  # ARGS[names(ARGS)==""]<-NULL # Argumente ohne Namen weg
  # for(arg in names(ARGS)) if(class(ARGS[arg][[1]])=="function") ARGS[arg][[1]]<-body(ARGS[arg][[1]])	
	
	# Sicherstellen, dass FUNC eine expression ist, weil substitute() nur expression nimmt
	if(is.function(func)) # Funktion: --> as.expression() geht nicht, also muss dieser Umweg sein!
		FUNC = parse(text=paste(deparse(FUNC,width.cutoff = 400L),collapse="")) # Wenn die Funktion eine Funktion ist, verwurste sie zu expression
	else # Es war keine Funktion --> einfach zu Expression machen
		FUNC = as.expression(FUNC) # Funktion zu expression machen

	# Die eigentliche Ersetzung der Variablen mit Werten
	FUNC = do.call(substitute,list(FUNC[[1]],ARGS))

	# Unnötig ? 
	# FUNC = parse(text=deparse(FUNC)) # Eine Expression draus machen (expression() geht nicht für Funktion)
	
	# War der Input eine Funktion? --> Dann soll Output auch eine Funktion sein
	if(is.function(func)) { 
		FUNC=eval(FUNC) # Eval machen, weil aus dem substitute() eine unevaluated expression rauskommt
		formals(FUNC)[intersect(names(formals(FUNC)),names(ARGS))] <- NULL # Argumente, die als Parameter ersetzt wurden, entfernen
		environment(FUNC) <- ENV # FUNC soll nicht im environment DIESER Funktion sein, sondern in dem aufrufenden Environment
	}
	
	return(FUNC) # Die ersetzte Funktion zurückgeben
}