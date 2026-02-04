#' @rdname internal_desc
#' @export
strat.collapse <- function(stratacnt, 
                           pltstratx, 
                           minplotnum.unit = 10,
                           minplotnum.unit.forest = FALSE,
                           minplotnum.strat = 2, 
                           minplotnum.strat.forest = FALSE,
                           unitarea, areavar, 
                           unitvar, unitvar2 = NULL, 
                           strvar,
                           getwt = FALSE, 
                           stratcombine = TRUE, 
                           unitcombine = FALSE, 
                           stratalevels = NULL,
                           vars2combine = NULL,  
                           UNITCD = NULL, ...) {
  ## unitcombine - If TRUE, combine estimation units, If FALSE, only combine strata

  ## Set global variables
  n.strata=n.total=puniqueid=unitstrgrplut=unitnew=strvarnew <- NULL
  addUNITCD <- FALSE

  if (!"data.table" %in% class(stratacnt)) stratacnt <- setDT(stratacnt)
  if (!"data.table" %in% class(unitarea)) unitarea <- setDT(unitarea)


  ## Stop and send message if stratcombine=FALSE
  ######################################################################################
  if (!stratcombine) {
    if (any(unique(stratacnt$n.total) < minplotnum.unit))
      message("estimation unit has less than ", minplotnum.unit, " plots",
		"... must combine estimation units")
    if ("n.strata" %in% names(stratacnt) &&
			any(unique(stratacnt$n.strata) < minplotnum.strat)) {
      stop("strata has less than ", minplotnum.strat, " plots",
		"... must combine strata")
    }
  }

  ## Stop and send message if unitcombine=FALSE and total plots less than minplotnum.unit
  #######################################################################################
  if (!unitcombine) {
    if (any(unique(stratacnt$n.total) < minplotnum.unit)) {
      estunits <- unique(stratacnt[stratacnt$n.total < minplotnum.unit, unitvar, with=FALSE][[1]])
      message("one or more estimation units has less than ", minplotnum.unit, " plots",
		         "... set unit_opts(unit.action) to remove or combine estimation units")
      messagedf(stratacnt[stratacnt[[unitvar]] %in% estunits,])
      stop()
    }
  }

  #############################################################################
  ## If stratcombine=TRUE and unitcombine=TRUE and number of total plots is less
  ## than minplotnum.unit.
  #############################################################################
  tabprint <- FALSE
  if (unitcombine && any(unique(stratacnt$n.total) < minplotnum.unit)) {
    tabprint <- TRUE
    message("\ncollapsing estimation units...")

    ## Define a variable to restrain collapsing by. Use unitvar2 if exists.
    if (is.null(unitvar2)) {
      if (unitvar != "UNITCD" && !"UNITCD" %in% names(stratacnt)) {
        stratacnt$UNITCD <- 1
        addUNITCD <- TRUE
      }
      unitcombinevar <- "UNITCD"
    } else {
      unitcombinevar <- unitvar2
    }
    
    ## define column to use to check number of plots
    nplotsvar <- ifelse (minplotnum.unit.forest, "n.stratafor", "n.strata")
      
    ## Group estimation units (by UNITCD) if less than minplotnum
    unitgrp <- stratacnt[, groupClasses(.SD, minplotnum = minplotnum.unit, 
                                        nvar = "n.total", xvar = unitvar,
                                        sumvar = nplotsvar,
                                        xvarlevels = NULL), by=UNITCD]
    setnames(unitgrp, "classnew", "unitnew")
  
    ## define collapsed unitvar as 'unitnew'
    unitvarnew <- "unitnew"
    SDcols <- c(vars2combine, "n.strata", "n.total")
    SDcols <- SDcols[SDcols %in% names(stratacnt)]
    unitgrpsum <- unitgrp[, lapply(.SD, sum, na.rm=TRUE),
			           by=c(unitcombinevar, unitvarnew, strvar), .SDcols=SDcols]
    setkeyv(unitgrpsum, c(unitcombinevar, unitvarnew, strvar))

    if (addUNITCD) {
      unitgrpsum[, (unitcombinevar) := NULL]
      unitjoinvars <- c(unitvar)
    } else {
      unitjoinvars <- c(unitcombinevar, unitvar)
    }

    ## Create look up table with original classes and new classes
    unitgrpvars <- c(unitjoinvars, unitvarnew)
    unitgrplut <- unique(unitgrp[, unitgrpvars, with=FALSE])
    unitstrgrplut <- unique(unitgrp[, c(unitgrpvars, strvar), with=FALSE])

    if (!is.null(unitarea)) {
      ## unitarea: Check if estunit1nm class match
      tabs <- check.matchclass(unitarea, unitgrplut, unitjoinvars)
      unitarea <- tabs$tab1
      unitgrplut <- tabs$tab2

      ## Merge new estimation unit to dat, unitarea, strlut
      unitarea <- merge(unitarea, unitgrplut, by=unitjoinvars)
      unitarea[, (unitvar) := NULL]
      unitvar <- unitvarnew
      unitarea <- unitarea[, sum(get(areavar)), by=unitvarnew]
      setnames(unitarea, "V1", areavar)
      setkeyv(unitarea, unitvarnew)
    }

    ## Merge new unitvar to pltstratx
    setkeyv(pltstratx, unitjoinvars)
    setkeyv(unitgrplut, unitjoinvars)

    ## Check that class of unitjoinvars in unitgrplut match plstratx
    matchcl <- check.matchclass(tab1=pltstratx, tab2=unitgrplut, matchcol=unitjoinvars,
		tab1txt="pltstrat", tab2txt="unitgrplut")
    pltstratx <- matchcl$tab1
    unitgrplut <- matchcl$tab2

    pltstratx <- merge(pltstratx, unitgrplut, by=unitjoinvars)
    unitvar <- unitvarnew

  } else {
    unitgrpsum <- stratacnt
    unitgrplut <- stratacnt
  }

  #############################################################################
  ## If stratcombine=TRUE and number of total plots is less than minplotnum.strat
  ## NOTE: minplotnum must not be greater than the minimum number
  ##		or plots by estimation unit plus 1.
  #############################################################################
  if ("n.strata" %in% names(unitgrpsum) && any(unique(unitgrpsum$n.strata) < minplotnum.strat)) {
    
    tabprint <- TRUE
    
    ## define column to use to check number of plots
    nplotsvar <- ifelse (minplotnum.strat.forest, "n.stratafor", "n.strata")
    
    ## Group strata (by unitvar) if less than minplotnum
    stratgrp <- unitgrpsum[, groupClasses(.SD, minplotnum = minplotnum.strat, 
                                        nvar = nplotsvar, xvar = strvar,
                                        sumvar = nplotsvar,
                                        xvarlevels = stratalevels), by=unitvar]
    setnames(stratgrp, "classnew", "stratnew")
    
    
    strlut <- stratgrp[, lapply(.SD, sum, na.rm=TRUE),
		      by=c(unitvar, "stratnew"), .SDcols=c(vars2combine, nplotsvar)]
    strlut[, n.total := stratgrp[match(strlut[[unitvar]], stratgrp[[unitvar]]),
		     "n.total"]]


    ## Create look up table with original classes and new classes
    unitstrjoinvars <- c(unitvar, strvar)
    if (!is.null(unitstrgrplut)) {
      unitstrgrplut <- merge(unitstrgrplut,
			stratgrp[, c(unitvar, strvar, "stratnew"), with=FALSE],
			by=unitstrjoinvars)
      unitstrgrplut <- unitstrgrplut[, c(unitgrpvars, strvar, "stratnew"), with=FALSE]
    } else {
      unitstrgrplut <- stratgrp[, c(unitvar, strvar, "stratnew"), with=FALSE]
    }
    ## Merge new strata to look up table with original classes and new classes
    keyvars <- unitstrjoinvars
    setkeyv(setDT(unitstrgrplut), keyvars)

    ## Merge new unitvar to pltstratx
    setkeyv(pltstratx, unitstrjoinvars)
    setkeyv(unitgrplut, unitvar)

    tabs <- check.matchclass(pltstratx, unitstrgrplut, unitstrjoinvars)
    pltstratx <- tabs$tab1
    unitstrgrplut <- tabs$tab2

    pltstratx <- merge(pltstratx,
		unique(unitstrgrplut[,c(unitstrjoinvars, "stratnew"), with=FALSE]),
		by=unitstrjoinvars)
    
    ## define collapsed strvar as 'stratnew'
    strvar <- "stratnew"
    strunitvars=c(unitvar, strvar)
    
  } else {
    strlut <- unitgrpsum
  }

  ## Print new table
#  if (tabprint) {
#    msg <- "## new stratalut"
#    message("\n################################### \n",
#            msg, "\n###################################")
#    message(paste0(capture.output(strlut), collapse = "\n"))
#  }
 
  returnlst <- list(pltstratx=pltstratx, strlut=strlut, unitvar=unitvar)
  if (!is.null(strvar)) returnlst$strvar <- strvar
  if (stratcombine && !is.null(unitstrgrplut)) {
    returnlst$unitstrgrplut <- unitstrgrplut
  }
  if (!is.null(unitarea)) {
    returnlst$unitarea <- unitarea
  }
  return(returnlst)
}


