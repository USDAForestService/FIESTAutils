#' Table aesthetics and output options.
#'
#' Returns a list of user-supplied parameters and parameter values for outputting
#' tables with custom aesthetics.
#'
#' If no parameters, an empty list is returned.
#'
#' @param row.FIAname Logical. If TRUE, retrieves default FIA reference names
#' for rowvar located in ref_codes data frame. Names are only available
#' for certain variables (Check sort(unique(ref_codes$VARIABLE)) for
#' available names.  If row.FIAname = TRUE and rowvar is in ref_codes,
#' the rowvar name is used for the output table, and the rowvar code is used to
#' sort.
#' @param col.FIAname Logical. If TRUE, retrieves default FIA reference names
#' for colvar located in ref_codes data frame. Names are only available
#' for certain variables. Check: sort(unique(ref_codes$VARIABLE)) for
#' available names.  If col.FIAname = TRUE and rowvar is in ref_codes,
#' the colvar name is used for the output table, and the colvar code is used to
#' sort.
#' @param row.orderby String. Optional. Name of variable to sort table rows.
#' Both the rowvar and row.orderby variables must be included in the same input
#' data.frame.  if NULL, and row.FIAname=FALSE or rowvar is not in
#' ref_codes, the rows are ordered by rowvar.
#' @param col.orderby String. Optional. Name of variable to sort table columns.
#' Both the colvar and col.orderby variables must be included in the same input
#' data.frame.  if NULL, and col.FIAname=FALSE or colvar is not in
#' ref_codes, the columns are ordered by colvar.
#' @param row.add0 Logical. If TRUE, include rows with 0 values to the output
#' table.
#' @param col.add0 Logical. If TRUE, include columns with 0 values to the
#' output table.
#' @param rowlut Data frame. A lookup table with variable codes and code names
#' to include as rows of output table (See notes for more information and
#' format).
#' @param collut Data frame. A lookup table with variable codes and code names
#' to include as columns of output table (See notes for more information and
#' format).
#' @param row.classify Data frame (if categorical) or Vector (if continuous). 
#' If clasifying categories, input a dataframe with two columns. The columns
#' must be named 'FROM' and 'TO', or include the rowvar and the classified 
#' column name. If classifying continuous values, input a vector of class breaks 
#' for row values (e.g., c(0,25,50): >=0 and < 25 (0-25); >=25 and <50 (25-50), 
#' >=50 (50+)).
#' @param col.classify Data frame (if categorical) or Vector (if continuous). 
#' If clasifying categories, input a dataframe with two columns. The columns
#' must be named 'FROM' and 'TO', or include the colvar and the classified 
#' column name. If classifying continuous values, input a vector of class breaks 
#' for row values (e.g., c(0,25,50): >=0 and < 25 (0-25); >=25 and <50 (25-50), 
#' >=50 (50+)).
#' @param rawonly Logical. If TRUE, only rawdata are output. If dataset
#' includes many estimation units, and only raw data tables are desired, it is
#' more efficient to output raw data only.
#' @param raw.keep0 Logical. If TRUE, keep 0 values in raw data tables.
#' @param rowgrp Logical. If TRUE, appends row groups to first column of table.
#' Only available if group category exists in ref_codes table or defined in
#' rowgrpnm (e.g., FORTYPGRPCD, OWNGRPCD).
#' @param rowgrpnm String. Name of variable for grouping rowvar. Variable must
#' be included in same input table as rowvar.
#' @param rowgrpord String. Name of variable to sort row group variable.
#' Variable must be included in same input table as rowgrpnm.
#' @param totals Logical. If TRUE, returns total estimate (mean * AREAUSED).
#' @param allin1 Logical. If TRUE, both estimates and percent sample error are
#' output in one table as: estimates (percent sample error).
#' @param metric Logical. If TRUE, output if returned in metric units.
#' @param estround Integer. Number of decimal places for estimates.
#' @param pseround Integer. Number of decimal places for percent sampling
#' error.
#' @param estnull Number or character. The number or symbol to use to indicate
#' 'not sampled' for estimate.
#' @param psenull Number or character. The number or symbol to use to indicate
#' 'not sampled' for percent standard error.
#' @param row.NAname String. The name to use for NA values for rows.
#' @param col.NAname String. String. The name to use for NA values for columns.
#' @param divideby String. Conversion number for output ('hundred', 'thousand',
#' 'million').
#' @param spcdname String. Type of name to use for species in tables ('COMMON', 
#' 'SCIENTIFIC', SYMBOL', 'COMMON_SCIENTIFIC', 'NONE').
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for outputting
#' tables with custom aesthetics.
#' @author Grayson W. White
#' @keywords options
#' @examples
#'
#' table_options(row.FIAname = TRUE, col.FIAname = TRUE)
#'
#' @export table_options

table_options <- 
  function(row.FIAname = FALSE, col.FIAname = FALSE, 
           row.orderby = NULL, col.orderby = NULL, 
					 row.add0 = FALSE, col.add0 = FALSE,
           rowlut = NULL, collut =NULL, 
					 row.classify = NULL, col.classify = NULL, 
					 rawonly = FALSE, raw.keep0 = FALSE,
           rowgrp = FALSE, rowgrpnm = NULL, rowgrpord = NULL,
           totals = TRUE, allin1 = FALSE, metric = FALSE, 
					 estround = 1, pseround = 2,
           estnull="--", psenull="--", 
					 row.NAname = "Other", col.NAname = "Other",
					 divideby = NULL, spcdname = "COMMON_SCIENTIFIC", ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(table_options)))
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
