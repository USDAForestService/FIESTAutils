#' Database - Test a PostgreSQL database.
#' 
#' Checks a PostgreSQL database.
#' 
#' @param dbname String. Name of the database on the host.
#' @param host String. Host name.
#' @param port String. Port number.
#' @param user String. User name.
#' @param password String. Password.
#' @param dbconnopen Logical. If TRUE, the database connection is returned and not closed.
#' @param showlist Logical. If TRUE, prints list of tables in database.
#' @param ... Additional authentication arguments passed to DBI::dbConnect
#' 
#' @return An S4 object that inherits from DBIConnection via the DBI package if
#' dbconnopen = TRUE, or NULL otherwise. 
#' For more information, see `help(DBI::dbConnect)`. 
#' @author Tracey S. Frescino
#' @keywords data
#' @export 
DBtestPostgreSQL <- function(dbname = NULL,
                             host = NULL,
                             port = NULL,
                             user = NULL,
                             password = NULL,
                             dbconnopen = FALSE,
                             showlist = TRUE,
                             ...) {
  
  check_conn <- DBI::dbCanConnect(drv = RPostgres::Postgres(),
                                  dbname = dbname,
                                  host = host,
                                  port = port,
                                  user = user,
                                  password = password,
                                  ...)
  
  if (check_conn) {
    message("Connection Successful")
    pg_conn <- DBI::dbConnect(drv = RPostgres::Postgres(),
                              dbname = dbname,
                              host = host,
                              port = port,
                              user = user,
                              password = password,
                              ...)
    tablst <- DBI::dbListTables(pg_conn)
  } else {
    if (!is.null(attr(check_conn, "reason"))) {
      stop(attr(check_conn, "reason"))
    } else {
      stop("PostgreSQL connection failed.")
    }
  }
  
  if (showlist) {
    message(paste0(tablst, collapse = "\n"))
  }
  if (dbconnopen) {
    out <- pg_conn
  } else {
    out <- NULL
    DBI::dbDisconnect(pg_conn)
  }
  
  return(out)
  
}



