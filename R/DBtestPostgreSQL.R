#' Database - Test a SQLite database table.
#' 
#' Checks a SQLite database.
#' 
#' 
#' @param dbname String. Name of PostgreSQL database.
#' @param dbconnopen Logical. If TRUE, the dbconn connection is not closed.
#' @return An S4 object that inherits from DBIConnection via the DBI package. 
#' For more information, see `help(DBI::dbConnect)`. 
#' @author Tracey S. Frescino
#' @keywords data
#' @export DBtestPostgreSQL
DBtestPostgreSQL <- function(dbname, dbconnopen=TRUE) {
  ## CONNECT TO ORACLE DATABASE
  ###########################################
  if (DBI::dbCanConnect(RPostgreSQL::PostgreSQL(), dbname=dbname)) {
    message("PostgreSQL connection successful")

    if (dbconnopen) {
      return(DBI::dbConnect(RPostgreSQL::PostgreSQL(), dbname=dbname))
    }     
  } else {
    stop("PostgreSQL connection failed")
  }
}
