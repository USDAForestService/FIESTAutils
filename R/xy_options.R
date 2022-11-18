#' List of population tables.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' xyuation (FIA or custom) extraction to be supplied to *DB functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param xy.uniqueid String. Unique identifier of xy.
#' @param xvar String. Name of variable in xy defining x coordinate.
#' @param yvar String. Name of variable in xy defining y coordinate.
#' @param xy.crs PROJ.4 String or CRS object or Integer EPSG code defining
#' Coordinate Reference System.
#' @param xyjoinid String. Name of variable in xy to join to plot data. 
#' If NULL, xyjoinid = xy.uniqueid.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Tracey S. Frescino
#' @keywords list
#' @examples
#' xy_options(xvar="LON", yvar="LAT")
#' @export xy_options

xy_options <- function(xy.uniqueid = "CN", 
                    xvar = "LON", 
                    yvar = "LAT", 
                    xy.crs = 4269, 
                    xyjoinid = NULL, 
                    ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(xy_options)))
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

