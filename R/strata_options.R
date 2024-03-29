#' Strata options.
#' 
#' Returns a list of user-supplied parameters and parameter values for strata.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param getwt Logical. If TRUE, calculates strata weights from stratatlut
#' getwtvar.  If FALSE, strwtvar variable must be in stratalut.
#' @param getwtvar String. If getwt=TRUE, name of variable in stratalut to
#' calculate weights (Default = 'P1POINTCNT').
#' @param strwtvar String. If getwt=FALSE, name of variable in stratalut with
#' calculated weights (Default = 'strwt').
#' @param stratcombine Logical. If TRUE, and strata=TRUE, automatically combines
#' strata categories if less than minplotnum.strat plots in any one stratum. 
#' See notes for more info.
#' @param minplotnum.strat Integer. Minimum number of plots for a stratum
#' within an estimation unit.
#' @param pivot Logical. If TRUE, pivot stratalut. 
#' @param nonresp Deprecated. 
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Grayson W. White
#' @keywords options
#' @examples
#' 
#' strata_options(getwt = FALSE)
#' 
#' @export strata_options

strata_options <- function(getwt = FALSE, 
                           getwtvar = "P1POINTCNT", 
                           strwtvar = "strwt",
                           stratcombine = TRUE, 
                           minplotnum.strat = 2, 
                           pivot = FALSE, 
                           nonresp = FALSE,
                           ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(strata_options)))

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

