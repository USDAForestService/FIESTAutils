#' Spatial - Exports a data frame object.
#'
#' Exports a data frame object to a specified output.
#'
#' Wrapper for sf::st_write function.
#'
#' @param dfobj Data.frame class R object. Data frame object to export.
#' @param create_dsn Boolean.
#' @param index.unique String. Name of variable(s) in dfobj to make unique
#' index.
#' @param index String. Name of variable(s) in dfobj to make (non-unique)
#' index.
#' @param lowernames Logical. If TRUE, convert column names to lowercase 
#' before writing to output.
#' @param savedata_opts List. See help(savedata_options()) for a list
#' of options.
#' @return An sf spatial object is written to the out_dsn.
#' @note If out_fmt='shp':\cr The ESRI shapefile driver truncates variable
#' names to 10 characters or less.  Variable names are changed before export
#' using an internal function (trunc10shp). Name changes are output to the
#' outfolder, 'outshpnm'_newnames.csv.
#'
#' If sf object has more than 1 record, it cannot be exported to a shapefile.
#' @author Tracey S. Frescino
#' @keywords data
#' @export datExportData
datExportData <- function(dfobj, 
                          create_dsn = FALSE,
                          index.unique = NULL, 
                          index = NULL, 
                          lowernames = FALSE,
                          savedata_opts = savedata_options()
                          ) {
  ###########################################################################
  ## DESCRIPTION: Exports a data.frame to file or database.
  ## out_fmt	Output format ('csv', 'sqlite', 'gpkg', 'shp', 'rda', 'rds', 'llo')
  ## out_dsn	Database file path (including extension or outfolder
  ## out_layer	Only include if out_dsn is a database (e.g., *.db, *.gdb)
  ##			If NULL, basename of out_dsn is used
  ## outfn.pre	Add a prefix to layer name
  ## index.unique String vector or List. A unique index containing one or more 
  ##                 variables in dfobj (e.g., c('PLT_CN', 'CONDID')) or
  ##                 a list of one or more unique indices (if out_fmt = "sqlite").
  ## index  String vector or List. A non-unique index containing one or more 
  ##             variables in dfobj (e.g., c('STATECD', 'UNITCD', 'COUNTYCD', 'PLOT'))
  ##             or a list of one or more non-unique indices (if out_fmt = "sqlite").
  ###########################################################################

  ##################################################################
  ## CHECK INPUT PARAMETERS
  ##################################################################

  ## IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  gui <- FALSE
  #gui <- ifelse(nargs() == 0, TRUE, FALSE)


  ## Check dfobj
  ###########################################################
  if (is.null(dfobj)) {
    dfnm <- select.list(ls(pos=1, all.names=TRUE), title="data frame object?",
		multiple=FALSE)
    dfobj <- get(dfnm)
  }
  if (!"data.frame" %in% class(dfobj)) {
    stop("the object must be of class data frame")
#  } else if ("data.table" %in% class(dfobj)) {
#    dfobj <- setDF(dfobj)
  }

  # Check input parameters
  input.params <- names(as.list(match.call()))[-1]
  formallst <- c(names(formals(datExportData)))
  if (!all(input.params %in% formallst)) {
    miss <- input.params[!input.params %in% formallst]
    stop("invalid parameter: ", toString(miss))
  }

  ## Check parameter lists
  pcheck.params(input.params, 
                savedata_opts = savedata_opts)

  ## Check parameter option lists
  optslst <- pcheck.opts(optionlst = list(
                         savedata_opts = savedata_opts))
  savedata_opts <- optslst$savedata_opts  
  

  ## Check output data and assign to objects
  outlst <- pcheck.output(savedata_opts = savedata_opts)
  for (i in 1:length(outlst)) {
    assign(names(outlst)[[i]], outlst[[i]])
  }


  if (!overwrite_layer && !append_layer) {
    append_layer <- TRUE
  }

  ## Write data frame
  ########################################################
  if (out_fmt %in% c("sqlite", "gpkg")) {
    gpkg <- ifelse(out_fmt == "gpkg", TRUE, FALSE)
 
    dbconn <- write2sqlite(setDT(dfobj), 
                           SQLitefn = out_dsn, 
                           lowernames = lowernames,
                           outfolder = outfolder, 
                           out_name = out_layer, gpkg = gpkg, 
                           overwrite = overwrite_layer, 
                           append_layer = append_layer,
                           index.unique = index.unique, 
                           index = index, 
                           createnew = create_dsn, 
                           dbconn = outconn, 
                           dbconnopen = outconnopen)

  } else if (out_fmt == "gdb") {
     stop("cannot write to a geodatabase")
#    message(out_layer, " written to ", out_dsn)
#    out_dsn <- DBtestESRIgdb(out_dsn, outfolder=outfolder,
#		overwrite=overwrite_dsn, outfn.date=outfn.date, showlist=FALSE)
#    arcgisbinding::arc.write(file.path(out_dsn, out_layer), dfobj,
#			overwrite=overwrite_layer)

  } else if (out_fmt == "csv") {
    write2csv(dfobj, 
              lowernames = lowernames,
              outfolder = outfolder, 
              outfilenm = out_layer, 
              outfn.pre = outfn.pre, 
              outfn.date = outfn.date, 
              overwrite = overwrite_layer, 
              appendfile = append_layer)

  } else if (out_fmt == "rda") {
    objfn <- getoutfn(outfn = out_layer, 
                      outfolder = outfolder, 
                      outfn.pre = outfn.pre, 
                      outfn.date = outfn.date, 
                      overwrite = overwrite_layer, 
                      ext =" rda")
    save(dfobj, file=objfn)
  } else if (out_fmt == "rds") {
    objfn <- getoutfn(outfn=out_layer, 
                      outfolder=outfolder, 
                      outfn.pre=outfn.pre, 
                      outfn.date = outfn.date, 
                      overwrite = overwrite_layer, 
                      ext = "rds")
    save(dfobj, file=objfn)
  } else if (out_fmt == "llo") {
    objfn <- getoutfn(outfn = out_layer, 
                      outfolder = outfolder, 
                      outfn.pre = outfn.pre, 
                      outfn.date = outfn.date, 
                      overwrite = overwrite_layer, 
                      ext = "llo")
    save(dfobj, file=objfn)
  } else {
    stop(out_fmt, " currently not supported")
  }
 

  if (!is.null(outlst$outconn) && DBI::dbIsValid(outlst$outconn)) {   
    if (!outconnopen) {
      DBI::dbDisconnect(outlst$outconn)
      return(NULL)
    } else {
      return(outlst$outconn)
    }
  }  
}
