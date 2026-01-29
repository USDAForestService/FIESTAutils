#' @rdname internal_desc
#' @export
check.pltcnt <- function(pltx, puniqueid=NULL, unitlut, unitvars=NULL,
	strvars=NULL, savedata=FALSE, outfolder=NULL, outfn=NULL, overwrite=FALSE,
	outfn.date=TRUE, outfn.pre=NULL, minplotnum.unit=10, minplotnum.strat=2,
	minplotnum.unit.forest=FALSE, minplotnum.strat.forest=FALSE,
	gui=FALSE, stopiferror=FALSE, showwarnings=TRUE) {

  ####################################################################################
  ## CHECKS NUMBER OF PLOTS BY ESTIMATION UNIT/STRATA
  ## If there are < 2 plots in combined strunitvars, an error occurs.. must collapse
  ## If there are 2-10 plots, a warning is displayed..  suggesting to collapse.
  ## If there are plots with no data in acrelut, an error occurs.
  ## If savedata=TRUE and any errors occurred, saves strata counts to outfolder
  ##	(stratacnt.csv).
  ## Appends to unitlut:
  ##   n.strata	- if strvars != NULL, number of plots by strata
  ##   n.total - total number of plots by strata
  ## Returns: unitlut
  ####################################################################################

  ## CHECK GUI - IF NO ARGUMENTS SPECIFIED, ASSUME GUI=TRUE
  #if (nargs() == 0) gui <- TRUE
  gui <- FALSE

  ## If gui.. set variables to NULL
  if (gui) getwt=savedata <- NULL

  ## Set global variables
  NBRPLOTS=NBRSTRATA=n.strata=n.total=TOTACRES=strwt=errtab=joinvars=nostrata <- NULL

  ###############################################################
  ## Parameter checks
  ###############################################################

  ## Check pltx
  pltx <- pcheck.table(pltx, gui=gui, tabnm="pltx")

  ## Check unitlut
  ############################################################################
  unitlut <- pcheck.table(unitlut, gui=gui, tabnm="unitlut",
 	caption="Strata table?", nullcheck=TRUE, stopifnull=TRUE)

  ## GET savedata, outfolder, outfn
  savedata <- pcheck.logical(savedata, varnm="savedata", title="Save data?",
		first="YES", gui=gui)

  if (savedata) {
    outfolder <- pcheck.outfolder(outfolder, gui)
    if (is.null(outfn)) outfn <- "stratacnt"
  }

  ###############################################################
  ## DO work
  ###############################################################
  strunitvars <- c(unitvars, strvars)
  pvars <- c("STATECD", "UNITCD")
  pvars2keep <- pvars[which(pvars %in% names(pltx))]
  pvars2keep <- pvars2keep[which(pvars2keep %in% names(unitlut))]
  cnt.total <- "n.total"
  cnt.strata <- "n.strata"
  
  
  ## Add number of plots by unit
  pltcnt <- pltx[, list(n.total=.N), by=unitvars]
  setkeyv(pltcnt, unitvars)
  
  ## combine total counts with unitlut
  setkeyv(unitlut, unitvars)
  unitlut <- merge(unitlut, pltcnt, by=unitvars, all.x=TRUE)
  ncols <- "n.total"
  
  ## Check number of forested plots by estimation unit
  if (minplotnum.unit.forest) {
    cnt.total <- "n.forest"
    
    plot_status_cdnm <- findnm("PLOT_STATUS_CD", names(pltx), returnNULL = TRUE)
    if (is.null(plot_status_cdnm)) {
      stop("need plot_status_cd to get number of forested plots")
    }
    #pltcnt.for <- pltx[, list(n.forest=sum(.SD)), by=unitvars, .SDcols = plot_status_cdnm]
    pltcnt.for <- pltx[, list(n.forest = sum(ifelse(get(plot_status_cdnm) == 1, 1, 0))), by=unitvars]
    
    ## combine total counts with unitlut
    setkeyv(unitlut, unitvars)
    pltcnt <- merge(pltcnt, pltcnt.for, by=unitvars, all.x=TRUE)
    unitlut <- merge(unitlut, pltcnt.for, by=unitvars, all.x=TRUE)
    ncols <- c(ncols, "n.forest")
  }

  
  if (!is.null(strvars)) {
    #setkeyv(pltcnt, strunitvars)
    setkeyv(unitlut, strunitvars)
    joinvars <- unique(c(pvars2keep, strunitvars))

    ## Get strata counts
    pltstrcnt <- pltx[, list(n.strata=.N), by=strunitvars]
    setkeyv(pltstrcnt, strunitvars)

    ## combine total counts and strata counts
    unitlut <- merge(unitlut, pltstrcnt, by=strunitvars, all.x=TRUE)
    ncols <- c(ncols, "n.strata")
    
    
    ## Check number of forested plots by strata
    if (minplotnum.strat.forest) {
      cnt.strata <- "n.stratafor"
      
      plot_status_cdnm <- findnm("PLOT_STATUS_CD", names(pltx), returnNULL = TRUE)
      if (is.null(plot_status_cdnm)) {
        stop("need plot_status_cd to get number of forested plots")
      }
      pltstrcnt.for <- pltx[, list(n.stratafor=sum(.SD)), by=strunitvars, .SDcols = plot_status_cdnm]
      pltstrcnt.for <- pltx[, list(n.stratafor = sum(ifelse(get(plot_status_cdnm) == 1, 1, 0))), 
                            by=strunitvars]
      
      ## combine total counts with unitlut
      unitlut <- merge(unitlut, pltstrcnt.for, by=strunitvars, all.x=TRUE)
      ncols <- c(ncols, "n.stratafor")
    }
    
    ## change NA values to 0 values
    unitlut <- DT_NAto0(unitlut, ncols)
    
 
    ## Get number of potential combinations of strata from unitlut
    pltcnt <- copy(unitlut)
    unitlutcnt <- pltcnt[, list(NBRSTRATA=.N), strunitvars]
    setkeyv(unitlutcnt, strunitvars)

    ## Merge number of plots by strata from pltx and number of potential combos
    pltcnt <- merge(pltcnt, unitlutcnt, all.x=TRUE, all.y=TRUE)
    pltcnt[is.na(pltcnt)] <- 0
    nostrata <- subset(pltcnt, NBRPLOTS > 0 & NBRSTRATA == 0)
    
    errtab <- copy(unitlut)
    errtab$errtyp <- "none"
    
    #pltcnt[pltcnt$n.strata < minplotnum.strat & pltcnt$n.total < minplotnum.unit
	#	& pltcnt$NBRSTRATA > 0, "errtyp"] <- "warn"
#    pltcnt[((pltcnt$n.strata < minplotnum.strat & pltcnt$n.total > minplotnum.unit) |
#		      pltcnt$n.total < minplotnum.unit), "errtyp"] <- "warn"
#    pltcnt[pltcnt$n.total < minplotnum.strat & pltcnt$NBRSTRATA > 0, "errtyp"] <- "warn"
    
    errtab[((errtab[[cnt.strata]] < minplotnum.strat & errtab[[cnt.total]] > minplotnum.unit) |
              errtab[[cnt.total]] < minplotnum.unit), "errtyp"] <- "warn"

    ## ## Remove NBRSTRATA and merge to unitlut
#    pltcnt[, NBRSTRATA:=NULL]
    pvars <- pvars[pvars %in% names(unitlut)]
    othervars <- names(unitlut)[!names(unitlut) %in% unique(c(pvars, strunitvars))]
    setcolorder(unitlut, c(unique(c(pvars, strunitvars)), othervars))
    setorderv(unitlut, unique(c(pvars, strunitvars)))

  } else {

    errtab <- copy(unitlut)
    errtab$errtyp <- "none"
    errtab[errtab$n.total < minplotnum.unit, "errtyp"] <- "warn"
    
    pvars <- pvars[pvars %in% names(unitlut)]
    othervars <- names(unitlut)[!names(unitlut) %in% unique(c(pvars, unitvars))]
    setcolorder(unitlut, c(unique(c(pvars, unitvars)), othervars))
    setorderv(unitlut, unique(c(pvars, unitvars)))
  }

  if (showwarnings && any(pltcnt$errtyp == "warn")) {
    msg <- "## warnings/errors"
    message("\n################################### \n",
            msg, "\n###################################")
    message(paste0(capture.output(data.frame(pltcnt[pltcnt$errtyp == "warn",], check.names=FALSE)), 
                   collapse = "\n"))
    message("not enough plots in one or more stratum")
    
    if (stopiferror && any(errtab[["errtyp"]] == "warn")) {
      stop("not enough plots in strata")
    }

    ## If savedata, write to file
    ###############################################################
    if (savedata) {
      write2csv(errtab, outfolder=outfolder, outfilenm=outfn, outfn.date=outfn.date,
		        outfn.pre=outfn.pre, overwrite=overwrite)
    }
  }

  returnlst <- list(unitlut=unitlut, errtab=errtab)

  if (!is.null(nostrata) && nrow(nostrata) > 0) {
    returnlst$nostrat <- nostrata
  }
  return(returnlst)
}
