#' Options for summarizing tree data.
#'
#' Returns a list of user-supplied parameters and parameter values for 
#' summarizing tree data.
#'
#' If no parameters, an empty list is returned.
#'
#' @param lbs2tons Logical. If TRUE, converts biomass or carbon variables from
#' pounds to tons (1 pound = 0.0005 short tons). If metric=TRUE, converts to  
#' metric tons, else short tons.
#' @param metric Logical. If TRUE, converts response to metric units based on
#' ref_conversion, if any variable in tsumvarlst is in
#' FIESTAutils::ref_estvar.  Note: if TPA, TPA is converted to trees per hectare
#' (TPH: (1/ tpavar * 0.4046860)).
#' @param tround Number. The number of digits to round to. If NULL, default=5.
#' @param TPA Logical. If TRUE, tsumvarlst variable(s) are multiplied by the
#' respective trees-per-acre variable (see details) to get per-acre
#' measurements.
#' @param adjTPA Numeric. A tree-per-acre adjustment. Use for DESIGNCD=1
#' (annual inventory), if using less than 4 subplots. If using only 1 subplot
#' for estimate, adjTPA=4. The default is 1.
#' @param ACI Logical. If TRUE, if ACI (All Condition Inventory) plots exist,
#' any trees on these plots will be included in summary. If FALSE, you must
#' include condition table.
#' @param adjtree Logical. If TRUE, trees are individually adjusted by
#' adjustment factors.  Adjustment factors must be included in tree table (see
#' adjvar).
#' @param adjvar String. If adjtree=TRUE, the name of the variable to use for
#' multiplying by adjustment (e.g., tadjfac).
#' @param keepall Logical. If TRUE, keeps all plots in dataset with NA values. 
#' If FALSE, keeps only summed data when not NA.  
#' @param NAto0 Logical. If TRUE, change NA values to 0
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for 
#' summarizing tree data.
#' @author Tracey S. Frescino
#' @keywords options
#' @examples
#'
#' datSum_options(lbs2tons = TRUE, metric = TRUE)
#'
#' @export datSum_options
datSum_options <- 
  function(lbs2tons = TRUE, 
           metric = FALSE, 
           tround = 5,
           TPA = TRUE,
           adjTPA = 1,
           ACI = FALSE,
           adjtree = FALSE,
           adjvar = "tadjfac",
           keepall = FALSE,
           NAto0 = TRUE,
           ...) {

  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(datSum_options)))
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
