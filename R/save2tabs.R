#' @rdname internal_desc
#' @export
save2tabs <- function(tab1, tab2, tab1.title, tab2.title, outfolder, coltitlerow=TRUE,
	coltitle=NULL, addtitle=TRUE, rowtotal=TRUE, rnames=NULL, outfn.estpse=NULL,
	outfn.date=TRUE, overwrite=FALSE){

  ## DESCRIPTION: To save 2 tables of estimates and standard errors to an Excel
  ## 		spreadsheet (*.xlsx) or a comma-delimited file (*.csv).
  ## ARGUMENTS:
  ## tab1  		Dataframe. Table of estimates.
  ## tab2    	Dataframe. Table of standard errors.
  ## tab1.title  String. The title for the table of estimates.
  ## tab2.title   String. The title for the table of standard errors.
  ## outfn.estpse String. The name of the output file (no extension).
  ## outfolder  String. The name of the output folder.
  ## coltitlerow Logical. If TRUE, there is a title for Columns

  ## Make sure tab1 and tab2 are data.tables
  tab1 <- pcheck.table(tab1)
  tab2 <- pcheck.table(tab2)

  ## REMOVE COLUMNS WITH 0 VALUES.
 # if (rowtotal) {
 #   tab1 <- tab1[, tab1[tab1[,1] == "Total",] != 0]
 #   tab2 <- tab2[, tab2[tab2[,1] == "Total",] != 0]
 # }

  ## Get number of row names
  rnbr <- length(rnames)

  ## ADD TITLE AND FORMAT TABLE
  ###########################################
  if (coltitlerow) {
    ## ADD TITLE TO TABLE COLUMNS AND FORMAT TABLE
    #tab1[,-(1:rnbr)] <- noquote(lapply(tab1[,-(1:rnbr)],
	#	function(x) {format(x[!is.na(suppressWarnings(as.numeric(x)))], big.mark=",")} ))
    tab1 <- tab1[, lapply(.SD, function(x) ifelse(is.na(suppressWarnings(as.numeric(x))),
		x, format(suppressWarnings(as.numeric(x)), big.mark=",")))]
    tab1 <- rbindlist(list(setDT(as.list(colnames(tab1)))[], tab1), use.names=FALSE)
    colnames(tab1) <- c(rep(" ", rnbr), coltitle, rep(" ", ncol(tab1)- (rnbr+1)))
    tab2b <- rbindlist(list(setDT(as.list(colnames(tab2)))[], tab2), use.names=FALSE)

    tab2 <- rbindlist(list(setDT(as.list(colnames(tab2)))[], tab2), use.names=FALSE)
    colnames(tab2) <- c(rep(" ", rnbr), coltitle, rep(" ", ncol(tab2)- (rnbr+1)))
  }

  ## Get outfile name
  outfilenm <- getoutfn(outfn=outfn.estpse, outfolder=outfolder,
	outfn.date=outfn.date, overwrite=overwrite, ext="csv")


  ## WRITE BOTH TABLES TO ONE FILE, SKIPPING 1 space.
  outfile <- file(outfilenm, "w")

  if (addtitle) {
    cat(gsub(",", "; ", tab1.title), file=outfile, sep="\n")
    cat("\n", file=outfile)
  } else {
    cat("Estimate", file=outfile, sep="\n")
  }
  outfile <- write2csv(tab1, outfile=outfile, appendfile=TRUE, closefn=FALSE)

  if (addtitle) {
    cat("\n", file=outfile)
    cat(gsub(",", "; ", tab2.title), file=outfile, sep="\n")
    cat("\n", file=outfile)
  } else {
    cat("", "Percent Error", file=outfile, sep="\n")
  }
  outfile <- write2csv(tab2, outfile=outfile, appendfile=TRUE, closefn=FALSE)
  close(outfile)

  message("###################################\n",
			"Table written to: ", outfilenm,
		"\n###################################")
}

