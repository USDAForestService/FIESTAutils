#' @rdname internal_desc
#' @export
getadjfactorDWM <- function(adj="samp", 
	condx, cuniqueid="PLT_CN", condid="CONDID", 
	unitlut=NULL, unitvars=NULL, strvars=NULL, unitarea=NULL, 
	areavar=NULL, areawt="SUBP_CONDPROP_UNADJ", dwmpropvars=NULL){
  ####################################################################################
  ## DESCRIPTION:
  ## Calculates adjustment factors for plots to account for nonsampled conditions.
  ## Creates an adjusted condition proportion by dividing 1 by summed proportions in plot.
  ## If adj="samp", n / summed condition proportions by strata (and estimation unit).
  ## If adj="plot, 1 / summed condition proportions by plot.
  ## VALUE:
  ##  1. Summed proportions (*PROP_UNADJ_SUM) and adjustment factors (*PROP_ADJFAC) by
  ##     strata and /or estunit (*PROP_UNADJ_SUM / n.strata or n.total, if strvars=NULL)
  ##  2. Adjusted condition proportion (CONDPROP_ADJ) appended to condx
  ####################################################################################

  ## Set global variables
  CONDPROP_ADJ=CONDPROP_UNADJ=ADJ_FACTOR_COND=expfac=
	expcond=expcondtab=n.strata=EXPNS=strwt=Prop <- NULL

  ## Define function
  adjnm <- function(nm) {
    ## DESCRIPTION: changes name of variable
    if (length(grep("UNADJ", nm)) == 1) {
      sub("UNADJ", "ADJ", nm)
    } else {
      paste0(nm, "_ADJ")
    }
  }

  ## Get key of condx
  keycondx <- key(condx)

  ## Condition proportion variable
  varlst <- areawt
  areasum <- paste0(areawt, "_SUM")
  areaadj <- paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", areawt))
  varsumlst <- areasum
  varadjlst <- areaadj
  cadjfacnm <- varadjlst


  dwmpropvars <- dwmpropvars[which(dwmpropvars%in% names(condx))]
  if (length(dwmpropvars) == 0) {
    stop("must include unadjusted variables in cond")
  }
  dwmvarsum <- lapply(dwmpropvars, function(x) paste0(x, "_SUM"))
  dwmvaradj <- lapply(dwmpropvars, function(x) sub("CONDPROP", "ADJ_FACTOR", x))
  varlst <- c(varlst, dwmpropvars)
  varsumlst <- c(varsumlst, unlist(dwmvarsum))
  varadjlst <- c(varadjlst, unlist(dwmvaradj))

 

  ###############################################################################
  ## Calculate adjustment factors by strata (and estimation unit) for variable list
  ## Sum condition variable(s) in varlst and divide by total number of plots in strata
  ###############################################################################

  if (adj == "samp") {
    ## check tables
    unitlut <- pcheck.table(unitlut)
    unitarea <- pcheck.table(unitarea)
    keyvars=strunitvars <- c(unitvars, strvars)
    setkeyv(unitlut, keyvars)

    ## Sum condition variable(s) in varlst by strata and rename varlst to *_sum
    cndadj <- condx[, lapply(.SD, sum, na.rm=TRUE), by=strunitvars, .SDcols=varlst]
    setnames(cndadj, varlst, varsumlst)
    setkeyv(cndadj, keyvars)

    ## Merge condition adjustment factors to strata table.
    unitlut <- unitlut[cndadj]
    n <- ifelse(is.null(strvars), "n.total", "n.strata")

    ## Calculate adjustment factor for conditions
    ## (divide summed condition proportions by total number of plots in strata)
    unitlut[, (varadjlst) := lapply(.SD,
	function(x, n) ifelse((is.na(x) | x==0), 0, get(n)/x), n), .SDcols=varsumlst]

    ## Merge condition adjustment factors to cond table to get plot identifiers.
    setkeyv(condx, keyvars)
    condx <- condx[unitlut[,c(strunitvars, varadjlst), with=FALSE]]

  } else {
    keyvars <- c(cuniqueid)
    setkeyv(condx, keyvars)

    ## Sum condition variable(s) in varlst by plot and rename varlst to *_sum
    pltadj <- condx[, lapply(.SD, sum, na.rm=TRUE), by=cuniqueid, .SDcols=varlst]
    setnames(pltadj, varlst, varsumlst)
    setkeyv(pltadj, keyvars)

    ## Calculate adjusted condition proportion for plots
    pltadj[, (varadjlst) := lapply(.SD,
	function(x) ifelse((is.na(x) | x==0), 0, 1/x)), .SDcols=varsumlst]
    condx <- condx[pltadj]
  }

  ## Calculate adjusted condition proportion for plots
  areawtnm <- adjnm(areawt)
  condx[, (areawtnm) := Map("*", mget(cadjfacnm), mget(areawt))]
  setkeyv(condx, c(cuniqueid, condid))

#  dwmwtnm <- adjnm(dwmpropvars)
#  condx[, (dwmwtnm) := Map("*", mget(dwmpropvars), mget(unlist(dwmvaradj)))]
#  setkeyv(condx, c(cuniqueid, condid))


  ## Calculate expansion factors (strata-level and cond-level)
  if (!is.null(unitarea)) {
    tabs <- check.matchclass(unitlut, unitarea, unitvars)
    unitlut <- tabs$tab1
    unitarea <- tabs$tab2

    ## Check if values match
    test <- check.matchval(unitlut, unitarea, unitvars, subsetrows=TRUE)
    if (nrow(test) < nrow(unitlut)) {
      stop("unitlut rows less than unitarea rows")
    }

    ## Merge unitlut with unitarea
    setkeyv(unitarea, unitvars)
    setkeyv(unitlut, unitvars)
    unitlut <- unitlut[unitarea]

    ## Expansion factors - average area by strata
    if (any(c("strwt", "Prop") %in% names(unitlut))) {
      if ("strwt" %in% names(unitlut)) {
        unitlut[, expfac:= get(areavar)/get(n)][, EXPNS := expfac * strwt]
      } else {
        unitlut[, expfac:= get(areavar)/get(n)][, EXPNS := expfac * Prop]
      }

      ## Condition-level expansion factors
      setkeyv(condx, strunitvars)
      expcondtab <- merge(condx, unitlut[,c(strunitvars, "EXPNS"), with=FALSE],
			by=strunitvars)
      expcondtab <- expcondtab[, expcond:=(CONDPROP_ADJ * EXPNS)][order(get(cuniqueid))][
    		, EXPNS := NULL][, (strunitvars) := NULL]
      setkeyv(condx, c(cuniqueid, condid))
      setkeyv(expcondtab, c(cuniqueid, condid))
    }

    setcolorder(unitlut, c(names(unitlut)[!names(unitlut) %in% varadjlst], 
			names(unitlut)[names(unitlut) %in% varadjlst]))  
  }

  ## Remove summed variables from condx
  vars2remove <- c(varsumlst)
  vars2removec <- vars2remove[vars2remove %in% names(condx)]
  if (length(vars2removec) > 0) {
    condx[, (vars2removec) := NULL]
  }
  if (adj == "samp") {
    vars2removeu <- vars2remove[vars2remove %in% names(condx)]
    unitlut[, (vars2remove) := NULL]
  }

  ## Remove *_ADJFAC and *_UNADJ columns in condx
  #condx[, names(condx)[grep("ADJ_FACTOR_", names(condx))]:= NULL]
  #condx[, names(condx)[grep("_UNADJ", names(condx))]:= NULL]

  adjfacdata <- list(condx=condx)

  if (adj == "samp") {
    setkeyv(unitlut, strunitvars)
    adjfacdata$unitlut <- unitlut
    adjfacdata$expcondtab <- expcondtab
  }

  adjfacdata$cvars2keep <- names(condx)[names(condx) != "CONDPROP_ADJ"]
  adjfacdata$areawtnm <- areawtnm 
  adjfacdata$varadjlst <- varadjlst

  return(adjfacdata)
}
