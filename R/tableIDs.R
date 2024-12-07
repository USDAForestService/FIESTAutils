#' List of FIADB table unique IDs.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' table unique IDs to be supplied to *pop functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param cond String. Unique identifier of plot in cond.
#' @param plt String. Unique identifier of plot in plt.
#' @param tree String. Unique identifier of plot in tree.
#' @param seedling String. Unique identifier of plot in seedling.
#' @param subplot String. Unique identifier of plot in subplot.
#' @param subp_cond String. Unique identifier of plot in subp_cond.
#' @param condid String. Unique identifier of a condition in cond.
#' @param subpid String. Unique identifier of a subplot in subplot and subp_cond.
#' @param ... For extendibility.
#' @return A list of user-supplied unique identifier of a plot in tables.
#' @author Tracey S. Frescino
#' @keywords list
#' @export tableIDs
 
tableIDs <- function(cond = "PLT_CN", 
                     plt = "CN", 
                     tree = "PLT_CN", 
						         seedling = "PLT_CN",
                     subplot = "PLT_CN",
                     subp_cond = "PLT_CN", 
						         condid = "CONDID",
						         subpid = "SUBP",
                     ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(tableIDs)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  # removes input parameters to create l correctly
  rm(input.params, formallst)
  
  # create list from input parameters
  l <- c(as.list(environment()), list(...))
  
  # return list
  return(l)
}

