#' Database options.
#' 
#' Returns a list of user-supplied parameters and parameter values for database access.
#' 
#' If no parameters, an empty list is returned.
#' 
#' @param dbname String. Name of database.
#' @param host String. Name of database host.
#' @param port String. Database port.
#' @param user String. User name for database access.
#' @param password String. Password for database access.
#' @param schema String. Name of schema in database.
#' @param dbconnopen Logical. If TRUE, keep database connection open.
#' @param ... For extendibility.
#' @return A list of user-supplied parameters and parameter values for saving data.
#' @author Tracey S. Frescino
#' @keywords options
#' @examples
#' 
#' savedata_options(outfolder = "path", overwrite_dsn = FALSE)
#' 
#' @export database_options
database_options <- function(dbname = NULL, 
                             host = NULL, 
                             port = NULL, 
                             user = NULL,
                             password = NULL,
                             schema = NULL,
                             dbconnopen = TRUE,
                             ...) {
  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(database_options)))
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

