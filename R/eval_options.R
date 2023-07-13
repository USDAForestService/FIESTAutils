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
#' @param Type String vector. Evaluation types ('ALL','CURR','VOL','P2VEG',
#' DWM','INV','CHNG','GRM','REGEN'). If eval='FIA', Type is equivalent to
#' plots for FIA Evaluations where 'ALL' includes nonsampled plots; 'CURR' 
#' and 'VOL' include plots used for area or tree estimates, respectively; 
#' Type = 'GRM' includes plots used for growth, removals, mortality; and 
#' Type = 'CHNG' includes plots used for change estimates (See FIA database 
#' manual for regioin availability and/or differences 
#' (https://www.fia.fs.usda.gov/library/database-documentation/index.php) 
#' If eval='custom', the associated tables are extracted for each Type. 
#' Multiple Types are accepted.
#' @param invyrs Integer vector. eval='custom': defines specific  
#' inventory years of data (e.g., 2010:2015). See FIA manual for 
#' definition of INVYR. 
#' @param measyrs Integer vector. eval='custom': defines specific
#' measurement years of data (e.g., 2010:2015).
#' @param evalType Deprecated. Use Type instead.
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
                         Type = "VOL",
                         evalid = NULL, 
                         invyrs = NULL, 
                         measyrs = NULL, 
                         evalType = "VOL",                        
                         ...) {

  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(eval_options)), "evalType")
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }
  
  # removes input parameters to create l correctly
  rm(input.params, formallst)
  
  # create list from input parameters
  l <- c(as.list(environment()), list(...))

  if ("evalType" %in% names(l)) {
    if ("Type" %in% names(l) && (length(Type) > 1 || l$Type != "VOL")) {
      l$Type <- l$Type
    } else {
      message("the parameter evalType is deprecated... use 'Type'\n")
      l$Type <- l$evalType 
    }
    l$evalType <- NULL
  } else {
    l$Type <- l$Type
  }

  
  # return list
  return(l)
}

