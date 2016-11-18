find_symbols <- function(expr) {
  functions <- variables <- character(0)

  f <- function(e) {
    if (!is.recursive(e)) {
      if (!is.symbol(e)) { # A literal of some type
        return()
      }
      variables <<- c(variables, deparse(e))
    } else {
      functions <<- c(functions, deparse(e[[1]]))
      for (a in as.list(e[-1])) {
        if (!missing(a)) {
          f(a)
        }
      }
    }
  }

  if (inherits(expr, "formula")) {
    expr <- expr[[length(expr)]]
  }
  f(expr)
  list(functions=unique(functions),
       variables=unique(variables))
}
