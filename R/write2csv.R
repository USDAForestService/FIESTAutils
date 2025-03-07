#' @rdname write2_desc
#' @export
write2csv <- function(layer, 
                      lowernames = FALSE,
                      outfile = NULL, 
                      outfolder = NULL, 
                      outfilenm = NULL,
                      outfn.pre = NULL, 
                      outfn.date = FALSE, 
                      overwrite = FALSE, 
                      tabtitle = NULL,
                      appendfile = FALSE, 
                      closefn = TRUE, 
                      outtxt = NULL, 
                      gui = FALSE){
  ###################################################################################
  ## DESCRIPTION: Internal function to write to csv file.
  ##
  ## ARGUMENTS:
  ##  layer    DF. The table to output.
  ##  outfile	An open outfile
  ##  outfilefn  String. The output file name (Full path) of open file or new file
  ##  outfn.date	Adds a date to the end of the file name
  ##  tabtitle  String. The title of the table.
  ##  outtxt	String. Name of file for printing to screen
  ##
  ## VALUE:
  ##  Writes out a table to a comma-delimited text file.
  ####################################################################################
  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  if (nargs() == 0) gui <- TRUE
  cnames <- TRUE
  appendtext <- ifelse(appendfile, "appended to", "written to")
  
  
  ## Check layer
  layer <- pcheck.table(layer)
  if (lowernames) {
    names(layer) <- tolower(names(layer))
  }
  

  if (is.null(outfile)) {
    ## Check outfilenm

    outfilenm <- getoutfn(outfilenm, outfolder=outfolder, outfn.pre=outfn.pre,
		outfn.date=outfn.date, overwrite=overwrite, outfn.default = "outfile",
		ext="csv", append=appendfile)
    ## open file
    if (appendfile) {
      outfile <- file(outfilenm, "a")
      cnames <- FALSE
    } else {
      outfile <- file(outfilenm, "w")
      cnames <- TRUE
    }

    msg <- ifelse (!is.null(outtxt) && is.character(outtxt),
		paste(outtxt, appendtext, outfilenm),
		paste("data frame", appendtext, outfilenm))

  } else if (!isOpen(outfile)) {
    stop("outfile is not an open file")
  } else {
    if (!is.null(outfilenm) && is.character(outfilenm)) {
      msg <- ifelse (!is.null(outtxt) && is.character(outtxt),
		  paste(outtxt, appendtext, outfilenm),
		  paste("data frame", appendtext, outfilenm))
    } else {
      msg <- ifelse (!is.null(outtxt) && is.character(outtxt),
        	paste(outtxt, appendtext, outfolder),
		  paste("data frame", appendtext, outfolder))
    }
  }


  ## If tabtitle is not null, add to file.
  if (!is.null(tabtitle))
    cat(tabtitle, file=outfile, sep="\n")

  ## Write table to file.
  suppressWarnings(write.table(layer, outfile, row.names=FALSE,
	           append=TRUE, sep=",", col.names=cnames))

  ## If closefn is TRUE, close the file.
  if (closefn) {
    close(outfile)

    message("################################### \n",
            msg, "\n###################################")

  } else {

    return(outfile)
  }
}
