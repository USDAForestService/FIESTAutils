#' @rdname write2_desc
#' @export
write2sqlite <- function(layer, SQLitefn=NULL, out_name=NULL, gpkg=FALSE,
 	outfolder=NULL, overwrite=FALSE, append_layer=FALSE, createnew=FALSE,
	dbconn=NULL, dbconnopen=FALSE, index.unique=NULL, index=NULL){
  ###################################################################################
  ## DESCRIPTION: Internal function to write to csv file.
  ##
  ## ARGUMENTS:
  ##  layer    DF. The table to output.
  ##  out_name	String. Name of output layer.
  ##  outfn.date	Adds a date to the end of the file name
  ##  index.unique String vector or List. A unique index containing one or more 
  ##                  variables in layer (e.g., c('PLT_CN', 'CONDID')) or
  ##                  a list of one or more unique indices.
  ##  index	String vector or List. A non-unique index containing one or more 
  ##               variables in layer (e.g., c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT'))
  ##               or a list of one or more non-unique indices.
  ##
  ## VALUE:
  ##  Writes data frame to SQLite database.
  ####################################################################################
  appendtext <- ifelse(append_layer, "appending", "writing")
 
  if (is.null(dbconn) || !DBI::dbIsValid(dbconn)) {
    ## Check SQLite connection
    ###########################################################
    if (createnew) {
      dbconn <- DBcreateSQLite(SQLitefn=SQLitefn, outfolder=outfolder, dbconnopen=TRUE,
		gpkg=gpkg, overwrite=overwrite)
    } else {
      dbconn <- DBtestSQLite(SQLitefn=SQLitefn, outfolder=outfolder, dbconnopen=TRUE,
		gpkg=gpkg, showlist=FALSE)
    }
  }

  ## Check out_name
  if (is.null(out_name))
    out_name <- "layer"

  ## Check layer
  layer <- pcheck.table(layer)


  ## Change columns of type POSIX* to character before writing to database
  if (any(grepl("POSIX", lapply(layer, class)))) {
    POSIXcols <- names(layer)[grepl("POSIX", lapply(layer, class))]
    layer <- setDF(layer)
    layer[POSIXcols] <- lapply(layer[POSIXcols], as.character)
    layer <- setDT(layer)
  }

  ## Write table to database
  DBI::dbWriteTable(dbconn, out_name, layer, append=append_layer, overwrite=overwrite)
  message(paste(appendtext, out_name, "to", SQLitefn))

  if (!all(index.unique %in% names(layer))) {
    warning("invalid index.unique... names not in ", out_name)
  }
  if (!is.null(index.unique)) {
    if (!is.list(index.unique)) {
      index.unique <- list(index.unique)
    }
    for (i in 1:length(index.unique)) {
      indexu <- index.unique[[i]]
      if (!all(indexu %in% names(layer))) {
        warning("invalid index.unique... names not in layer: ", toString(indexu))
      } else {
        indxunm <- paste0(out_name, "_", paste(tolower(indexu), collapse="_"), "_idx")
        if (sum(duplicated(layer[,indexu, with=FALSE])) > 0) {
          warning(indxunm, " is not unique... creating non-unique index\n")
          idxsql <- paste0("create index ", indxunm, " ON ", out_name,
				"(", paste(indexu, collapse=","), ")")
        } else {
          idxsql <- paste0("create unique index ", indxunm, " ON ", out_name,
				"(", paste(indexu, collapse=","), ")")

          test <- tryCatch(
            DBI::dbExecute(dbconn, idxsql),
		    error=function(err) {
				message(err, "\n")
		    } )
          if (!is.null(test)) {
            message(sub("create", "creating", idxsql))
          }
        }
      }
    }
  }
  if (!is.null(index)) {
    if (!is.list(index)) {
      index <- list(index)
    }
    for (i in 1:length(index)) {
      indexi <- index[[i]]
      if (!all(indexi %in% names(layer))) {
        warning("invalid index... names not in layer: ", toString(indexi))
      } else {

        indxnm <- paste0(out_name, "_", paste(tolower(indexi), collapse="_"), "_idx")
        message("adding index: ", indxnm, " to ", out_name)
        idxsql <- paste0("create index ", indxnm, " ON ",
				out_name, "(",  paste(indexi, collapse=","), ")")
        test <- tryCatch(
          DBI::dbExecute(dbconn, idxsql),
		    error=function(err) {
				message(err, "\n")
		    } )
        if (!is.null(test)) {
          message(sub("create", "creating", idxsql))
        }
      }
    }
  }
 
  ## If closedb is TRUE, close the sql database dbconnection.
  if (!is.null(dbconn)) {
    if (!dbconnopen) {
      DBI::dbDisconnect(dbconn)
      return(NULL)
    } else {
      return(dbconn)
    }
  }  
}
