#' @rdname internal_desc
#' @export
getadjfactorVOL <- function(adj=adj, condx=NULL, treex=NULL, seedx=NULL, 
     	tuniqueid="PLT_CN", cuniqueid="PLT_CN", condid="CONDID", unitlut=NULL, 
     	unitvars=NULL, strvars=NULL, unitarea=NULL, areavar=NULL, 
	areawt="CONDPROP_UNADJ", cvars2keep=NULL,
	cpropvars=list(SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ"),
	tpropvars=list(SUBP="SUBPPROP_UNADJ", MACR="MACRPROP_UNADJ", MICR="MICRPROP_UNADJ")){

  ####################################################################################
  ## DESCRIPTION:
  ## Calculates adjustment factors for area and trees by strata (and estimation unit)
  ##		to account for nonsampled plots and conditions.
  ## Creates an adjusted condition proportion by merging strata-level adjustment
  ##		factors to cond and dividing CONDPROP_UNADJ by adjustment factor.
  ## NOTE: The following variables must be included in your dataset:
  ##    unitvar (if there is more that 1 estimation unit)
  ##    strvar (defining strata)
  ##    CONDID;
  ##    TPA_UNADJ;
  ##    SUBPPROP_UNADJ (if you have TPA_UNADJ values > 5 and < 10);
  ##    MICRPROP_UNADJ (if you have TPA_UNADJ values > 50);
  ##    MACRPROP_UNADJ (if you have TPA_UNADJ values < 5)
  ##
  ## VALUE:
  ##  1. Summed proportions (*PROP_UNADJ_SUM) and adjustment factors (*PROP_ADJFAC) by
  ##     strata and /or estunit (*PROP_UNADJ_SUM / n.strata or n.total, if strvars=NULL)
  ##  2. Adjusted condition proportion (CONDPROP_ADJ) appended to condx
  ####################################################################################

  ## Set global variables
  CONDPROP_ADJ=CONDPROP_UNADJ=ADJ_FACTOR_COND=cadjfac=tadjfac=TPAGROW_UNADJ=
	ADJ_FACTOR_MICR=ADJ_FACTOR_MACR=ADJ_FACTOR_SUBP=expfac=expcond=expcondtab=
	n.strata=TPROP_BASIS=EXPNS=strwt=Prop=TPA_UNADJ <- NULL

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

  ## Get list of condition-level variables to calculate adjustments for
  if (!is.null(cpropvars)) {
    cvarlst <- unlist(cpropvars)
    cvarlst2 <- cvarlst[which(cvarlst%in% names(condx))]

    if (length(cvarlst2) > 0) {
      cvarsum <- lapply(cpropvars, function(x) paste0(x, "_SUM"))
      cvaradj <- lapply(cpropvars, function(x) paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)))
      varlst <- unique(c(varlst, cvarlst))
      varsumlst <- unique(c(varsumlst, unlist(cvarsum)))
      varadjlst <- unique(c(varadjlst, unlist(cvaradj)))
    }
  }

  ## Get list of condition-level variables to calculate adjustments for
  if (!is.null(treex)) {
    tvarlst <- unlist(tpropvars)
    tvarlst2 <- tvarlst[which(tvarlst%in% names(condx))]

    if (length(tvarlst2) == 0) {
      stop("must include unadjusted variables in cond")
    }
    tvarsum <- lapply(tvarlst, function(x) paste0(x, "_SUM"))
    tvaradj <- lapply(tvarlst, function(x) paste0("ADJ_FACTOR_", sub("PROP_UNADJ", "", x)))
    varlst <- unique(c(varlst, tvarlst))
    varsumlst <- unique(c(varsumlst, unlist(tvarsum)))
    varadjlst <- unique(c(varadjlst, unlist(tvaradj)))
  }
 
  ###############################################################################
  ## Calculate adjustment factors by strata (and estimation unit) for variable list
  ## Sum condition variable(s) in varlst and divide by total number of plots in strata
  ###############################################################################
  n <- "n.total"
  keyvars=strunitvars <- c(unitvars, strvars)

  if (adj == "samp") {
 
    ## check tables
    unitlut <- pcheck.table(unitlut)
    unitarea <- pcheck.table(unitarea)
    setkeyv(unitlut, keyvars)

    ## Sum condition variable(s) in varlst by strata and rename varlst to *_sum
    cndadj <- condx[, lapply(.SD, sum, na.rm=TRUE), by=strunitvars, .SDcols=varlst]
    setnames(cndadj, varlst, varsumlst)
    setkeyv(cndadj, keyvars)

    ## Merge condition adjustment factors to strata table.
    unitlut <- unitlut[cndadj]
    if (!is.null(strvars)) n <- "n.strata"

    ## Calculate adjustment factor for conditions
    ## (divide summed condition proportions by total number of plots in strata)
    unitlut[, (varadjlst) := lapply(.SD,
	function(x, n) ifelse((is.na(x) | x==0), 0, get(n)/x), n), .SDcols=varsumlst]

    ## Merge condition adjustment factors to cond table to get plot identifiers.
    setkeyv(condx, keyvars)
    condx <- condx[unitlut[,c(strunitvars, varadjlst), with=FALSE]]

    ## Change name of condition adjustment factor to cadjfac
    ## Note: CONDPPROP_UNADJ is the same as below (combination of MACR and SUBP)
#    if (all(length(areawt) == 1, areawt == "CONDPROP_UNADJ")) {  
#      cadjfacnm <- suppressMessages(checknm("cadjfac", names(condx)))
#      setnames(condx, areaadj, cadjfacnm)
#      setnames(unitlut, areaadj, cadjfacnm)
#
#      ## Calculate adjusted condition proportion for plots
#      areawtnm <- adjnm(areawt)
#      condx[, (areawtnm) := get(areawt) * get(cadjfacnm)]
#    } else {
#      cadjfacnm <- varadjlst

      ## Calculate adjusted condition proportion for plots
      areawtnm <- adjnm(areawt)
      condx[, (areawtnm) := Map("*", mget(cadjfacnm), mget(areawt))]
#    }
    setkeyv(condx, c(cuniqueid, condid))

    ## Calculate adjusted condition proportions for different size plots for trees
    if (!is.null(treex)) {
      setkeyv(treex, c(tuniqueid, condid))
      setkeyv(condx, c(cuniqueid, condid))

      ## Merge condition adjustment factors to tree table to get plot identifiers.
      ## Define a column in tree table, adjfact, to specify adjustment factor based on
      ##	the size of the plot it was measure on (identified by TPA_UNADJ)
      ## (SUBPLOT: TPA_UNADJ=6.018046; MICROPLOT: TPA_UNADJ=74.965282; MACROPLOT: TPA_UNADJ>6

      if ("TPROP_BASIS" %in% names(treex)) {
        treex[condx, tadjfac := ifelse(TPROP_BASIS == "MICR", get(tvaradj[["MICR"]]),
		ifelse(TPROP_BASIS == "MACR", get(tvaradj[["MACR"]]),
		get(tvaradj[["SUBP"]])))]
      } else {
        treex[condx, tadjfac := 1]
      }
      if (!is.null(seedx)) {
        setkeyv(seedx, c(tuniqueid, condid))
        seedx[condx, tadjfac := get(tvaradj[["MICR"]])]
      }
    }

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

    ## Change name of condition adjustment factor to cadjfac
    ## Note: CONDPPROP_UNADJ is the same as below (combination of MACR and SUBP)
    cadjfacnm <- suppressMessages(checknm("cadjfac", names(condx)))
    setnames(condx, areaadj, cadjfacnm)

    ## Calculate adjusted condition proportion for plots
    areawtnm <- adjnm(areawt)
    condx[, (areawtnm) := get(areawt) * get(cadjfacnm)]
    setkeyv(condx, c(cuniqueid, condid))


    ## Calculate adjustment factors for different size plots for trees
    if (!is.null(treex)) {
      ## Merge condition adjustment factors to tree table to get plot identifiers.
      ## Define a column in tree table, adjfact, to specify adjustment factor based on
      ##	the size of the plot it was measure on (identified by TPA_UNADJ)
      ## (SUBPLOT: TPA_UNADJ=6.018046; MICROPLOT: TPA_UNADJ=74.965282; MACROPLOT: TPA_UNADJ=0.999188
      setkeyv(treex, tuniqueid)

      if ("TPROP_BASIS" %in% names(treex)) {
        treex[pltadj, tadjfac := ifelse(TPROP_BASIS == "MICR", get(tvaradj[["MICR"]]),
		ifelse(TPROP_BASIS == "MACR", get(tvaradj[["MACR"]]),
		get(tvaradj[["SUBP"]])))]
      } else {
        treex[pltadj, tadjfac := ifelse(TPA_UNADJ > 50, get(tvaradj[["MICR"]]),
 		ifelse(TPA_UNADJ > 0 & TPA_UNADJ < 5, get(tvaradj[["MACR"]]),
 		get(tvaradj[["SUBP"]])))]
      }
      treex[, tadjfac := ifelse(tadjfac > 0, tadjfac, 1)]

      if (!is.null(seedx)) {
        setkeyv(seedx, c(tuniqueid))
        seedx[pltadj, tadjfac := get(tvaradj[["MICR"]])]
        seedx[, tadjfac := ifelse(tadjfac > 0, tadjfac, 1)]
      }
    }
  }

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
  if (!is.null(treex)) adjfacdata$treex <- treex
  if (!is.null(seedx)) adjfacdata$seedx <- seedx

  setkeyv(unitlut, strunitvars)    
  adjfacdata$unitlut <- unitlut
  adjfacdata$expcondtab <- expcondtab

  adjfacdata$cvars2keep <- names(condx)[names(condx) != "CONDPROP_ADJ"]
  adjfacdata$areawtnm <- areawtnm
  adjfacdata$areaadj <- areaadj
  adjfacdata$varadjlst <- varadjlst

  return(adjfacdata)
}
