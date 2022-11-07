#' List of population tables.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' evaluation (FIA or custom) extraction to be supplied to *DB functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param evalid Integer. To extract data for a specific evaluation period. See
#' notes for more information about FIA Evaluations.
#' @param evalCur Logical. If TRUE, extract plots with most current FIA
#' Evalidation for state(s).
#' @param evalEndyr Integer. Defining end year of Evaluation (yyyy).
#' @param evalAll Logical. All Evaluations in database.
#' @param evalType String vector. The type(s) of evaluation of interest ('ALL',
#' 'AREAVOL', 'GRM', 'P2VEG', 'DWM', 'INV', 'REGEN', 'CRWN').  The evalType
#' 'ALL' includes nonsampled plots; 'AREAVOL' includes plots used for area or
#' tree estimates (eval_typ %in% c(CURR, VOL)); The evalType 'GRM' includes
#' plots used for growth, removals, mortality, and change estimates (eval_typ
#' %in% c(GROW, MORT, REMV, CHNG)). Multiple types are accepted.  See FIA
#' database manual for regional availability and/or differences.
#' @param measCur Logical. If TRUE, extract plots with most current measurement
#' for state(s).
#' @param measEndyr Integer year (YYYY). If measCur=TRUE, extract plots with
#' most current measurement for state(s) for years measured before measEndyr.
#' @param invyrs Integer vector. Defining specific inventory years of data
#' (e.g., 2010:2015).
#' @param measyrs Integer vector. Defining specific measurement years of data
#' (e.g., 2010:2015).
#' @param allyrs Logical. If TRUE, selects all years (annual inventory) in
#' database.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Tracey S. Frescino
#' @keywords list
#' @examples
#' eval_options(measCur = TRUE)
#' @export eval_options

eval_options <- function(evalid = NULL,
                           evalCur = FALSE,
                           evalEndyr = NULL,
                           evalAll = FALSE,
                           evalType = "VOL",
                           measCur = FALSE,
                           measEndyr = NULL, 
                           invyrs = NULL, 
                           measyrs = NULL, 
                           allyrs = FALSE, 
                           ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(eval_options)))
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

