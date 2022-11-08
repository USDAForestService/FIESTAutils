#' List of population tables.
#' 
#' Returns a list of user-supplied parameters and parameter values for data 
#' evaluation (FIA or custom) extraction to be supplied to *DB functions. 
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param Cur Logical. If eval='FIA': extract plots with most current 
#' evaluation. If eval='custom': extract the most current sampled plots 
#' in the database.
#' @param Endyr Integer (YYYY). If eval='FIA', defines end year for  
#' extracting one or more FIA evaluation. If eval='custom', defines 
#' end year for extracting the most current sampled plots until. 
#' @param Endyr.filter Filter. If endyr != NULL, a filter to identify
#' when to use measEndyr, such as areas or plots identified as being 
#' disturbed in a particular year. In this example, plots sampled after
#' the disturbance will be excluded.
#' @param All Logical. If eval='FIA': includes all evaluations in 
#' database (annual inventory only). If eval='custom': includes all years 
#' in database (annual inventory only).
#' @param evalid Integer. Only eval='FIA': extract data for a specific 
#' evaluation period. See notes for more information about FIA Evaluations.
#' @param evalType String vector. Only eval='FIA": type(s) of 'FIA' 
#' evaluation ('CURR','VOL','GRM','P2VEG','DWM','INV','CHNG','GRM','REGEN').  
#' The evalType 'CURR' includes nonsampled plots; 'VOL' includes plots used 
#' for area or tree estimates (eval_typ %in% c(CURR, VOL)); The evalType 
#' 'GRM' includes plots used for growth, removals, mortality, and change 
#' estimates (eval_typ in(GROW, MORT, REMV, CHNG)). Multiple types are 
#' accepted. See FIA database manual for regional availability and/or 
#' differences.
#' @param invyrs Integer vector. eval='custom': defines specific  
#' inventory years of data (e.g., 2010:2015). See FIA manual for 
#' definition of INVYR. 
#' @param measyrs Integer vector. eval='custom': defines specific
#' measurement years of data (e.g., 2010:2015).
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for strata.
#' @author Tracey S. Frescino
#' @keywords list
#' @examples
#' eval_options(invyrs = 2015:2018)
#' @export eval_options

eval_options <- function(Cur = FALSE, 
                         Endyr = NULL,
                         Endyr.filter = NULL,
                         All = FALSE,
                         evalType = "VOL",
                         evalid = NULL, 
                         invyrs = NULL, 
                         measyrs = NULL,                         
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

