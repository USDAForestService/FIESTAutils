## DBvars.default	## Set default variable lists for extracting data from database
## DBgetfn			## Gets file name for Plot and Strata files.
## getspconddat
## getpfromqry      	## Get pfromqry for extracting data
## getplotCur
## getEvalid.ppsa     ## Get Evalid from pop_plot_stratum_assgn
## gui_filterdf		## Get filter from a data frame
## DBgetbyids		## Gets data from database from ids
## changeclass
## customEvalchk


#' @rdname internal_desc
#' @export
DBvars.default <- function(istree=FALSE, isseed=FALSE, isveg=FALSE, isgrm=FALSE, 
	isdwm=FALSE, issubp=FALSE, regionVars=FALSE, plotgeom=FALSE, regionVarsRS="RMRS") {

  ## Set global variables
  treevarlst=tsumvarlst=seedvarlst=ssumvarlst=vsubpsppvarlst=vsubpstrvarlst=
	dwmlst=pgeomvarlst <- NULL

  ## DESCRIPTION: Set default variables for FIA plot data extraction

  ########################################################################
  ##  DEFINE VARIABLES
  ########################################################################
  volvars <- c("VOLCFNET", "VOLCFGRS", "VOLBFNET", "VOLBFGRS", "VOLCSNET", "VOLCSGRS")
  growvars <- c("GROWCFGS", "GROWCFAL", "FGROWCFGS", "FGROWCFAL")
  mortvars <- c("MORTCFGS", "MORTCFAL", "FMORTCFGS", "FMORTCFAL")
  remvars <- c("REMVCFGS", "REMVCFAL", "FREMVCFGS", "FREMVCFAL")
  tpavars <- c("TPA_UNADJ", "TPAGROW_UNADJ", "TPAMORT_UNADJ", "TPAREMV_UNADJ")
  biovars <- c("DRYBIO_BOLE", "DRYBIO_STUMP", "DRYBIO_TOP", "DRYBIO_SAPLING",
		"DRYBIO_WDLD_SPP", "DRYBIO_BG", "DRYBIO_AG")
  carbonvars <- c("CARBON_BG", "CARBON_AG")


  ################################  PLOT VARIABLES ##################################
  ## Variables from FIADB.PLOT
  pltvarlst <- c("CN", "PREV_PLT_CN", "INVYR", "STATECD", "CYCLE", "SUBCYCLE",
	"UNITCD", "COUNTYCD", "PLOT", "PLOT_STATUS_CD", "PLOT_NONSAMPLE_REASN_CD",
	"SAMP_METHOD_CD", "SUBP_EXAMINE_CD", "MANUAL", "MACRO_BREAKPOINT_DIA",
	"INTENSITY", "MEASYEAR", "MEASMON", "MEASDAY", "REMPER", "KINDCD", "DESIGNCD",
	"RDDISTCD", "WATERCD", "LON", "LAT", "ELEV", "GROW_TYP_CD", "MORT_TYP_CD",
	"P2PANEL", "P3PANEL", "SUBPANEL", "DECLINATION",
	"NF_PLOT_STATUS_CD", "NF_PLOT_NONSAMPLE_REASN_CD", "NF_SAMPLING_STATUS_CD",
 	"P2VEG_SAMPLING_STATUS_CD", "P2VEG_SAMPLING_LEVEL_DETAIL_CD",
	"INVASIVE_SAMPLING_STATUS_CD", "INVASIVE_SPECIMEN_RULE_CD", "DESIGNCD_P2A",
	"QA_STATUS", "MODIFIED_DATE")

  if (regionVars && regionVarsRS == "RMRS") {
    pltvarlst <- c(pltvarlst, "COLOCATED_CD_RMRS", "CONDCHNGCD_RMRS",
		"FUTFORCD_RMRS", "MANUAL_RMRS", "PREV_PLOT_STATUS_CD_RMRS")
  }

  ################################  COND VARIABLES  ##################################
  ## Variables from FS_NIMS_FIADB_RMRS.COND
  condvarlst <- c("PLT_CN", "CONDID", "COND_STATUS_CD", "COND_NONSAMPLE_REASN_CD",
	"RESERVCD", "OWNCD", "OWNGRPCD", "FORINDCD", "ADFORCD", "FORTYPCD", "FLDTYPCD",
	"MAPDEN", "STDAGE", "STDSZCD", "FLDSZCD", "SITECLCD", "SICOND", "SIBASE", "SISP",
	"STDORGCD", "STDORGSP", "PROP_BASIS", "CONDPROP_UNADJ", "MICRPROP_UNADJ",
	"SUBPPROP_UNADJ", "MACRPROP_UNADJ", "SLOPE", "ASPECT", "PHYSCLCD", "GSSTKCD", "ALSTKCD",
	"DSTRBCD1", "DSTRBYR1", "DSTRBCD2", "DSTRBYR2", "DSTRBCD3", "DSTRBYR3",
	"TRTCD1", "TRTYR1", "TRTCD2", "TRTYR2", "TRTCD3", "TRTYR3", "PRESNFCD",
	"BALIVE", "FLDAGE", "FORTYPCDCALC", "HABTYPCD1", "HABTYPCD2", "LIVE_CANOPY_CVR_PCT",
	"LIVE_MISSING_CANOPY_CVR_PCT", "CANOPY_CVR_SAMPLE_METHOD_CD",
	"CARBON_DOWN_DEAD", "CARBON_LITTER", "CARBON_SOIL_ORG", "CARBON_STANDING_DEAD",
	"CARBON_UNDERSTORY_AG", "CARBON_UNDERSTORY_BG", "NF_COND_STATUS_CD",
	"NF_COND_NONSAMPLE_REASN_CD","LAND_COVER_CLASS_CD")

  if (regionVars && regionVarsRS == "RMRS") {
    condvarlst <- c(condvarlst, "LAND_USECD_RMRS", "PCTBARE_RMRS",
		"CRCOVPCT_RMRS", "COND_STATUS_CHNG_CD_RMRS", "QMD_RMRS",
		"RANGETYPCD_RMRS", "SDIMAX_RMRS", "SDIPCT_RMRS", "SDI_RMRS")
  }
  returnlst <- list(pltvarlst=pltvarlst, condvarlst=condvarlst)

  if (plotgeom) {
    ################################  PLOTGEOM VARIABLES ##################################
    ## Variables from FIADB.PLOTGEOM
    pgeomvarlst <- c("CN", "CONGCD", "ECOSUBCD", "HUC", "EMAP_HEX", "ALP_ADFORCD",
		"FVS_VARIANT", "FVS_LOC_CD", "FVS_REGION", "FVS_FOREST", "FVS_DISTRICT")
    returnlst$pgeomvarlst <- pgeomvarlst
  }


  ################################  TREE VARIABLES  ##################################
  if (istree) {

    ## Variables from FS_NIMS_FIADB_RMRS.TREE (these variables change from NIMSf to FIADB)
    treevarlst <- c("CN", "PLT_CN", "PREV_TRE_CN", "SUBP", "TREE", "CONDID",
		"AZIMUTH", "DIST", "STATUSCD", "SPCD", "SPGRPCD", "DIA", "HT", "ACTUALHT",
		"HTCD", "TREECLCD", "CR", "CCLCD", "AGENTCD", "CULL", "DECAYCD", "STOCKING",
		"WDLDSTEM", "MORTYR", "UNCRCD", "BHAGE", "TOTAGE", "MORTCD", "MIST_CL_CD",
		"STANDING_DEAD_CD", "PREV_STATUS_CD", "PREV_WDLDSTEM", "RECONCILECD", "PREVDIA")

    ## Tree summary variables
    tsumvarlst <- c(volvars, growvars, mortvars, remvars, tpavars, biovars, carbonvars)

    if (regionVars && regionVarsRS == "RMRS") {
      treevarlst <- c(treevarlst, "TREECLCD_RMRS", "DAMAGE_AGENT_CD1",
		"DAMAGE_AGENT_CD2", "DAMAGE_AGENT_CD3", "RADGRW_RMRS", "DIA_1YRAGO_RMRS",
		"HT_1YRAGO_RMRS", "PREV_ACTUALHT_RMRS", "PREV_TREECLCD_RMRS")
    }

    ## Variables to convert from NA to 0
#    treenavars <- c(tsumvarlst, "TOTAGE", "BHAGE", "CULLDEAD", "CULLFORM", "CFSND",
#		"CULLCF", "MIST_CL_CD", "DAMLOC1", "DAMTYP1", "DAMSEV1", "DAMLOC2", "DAMTYP2",
#		"DAMSEV2", "STOCKING", "RADGRW_RMRS")


    returnlst$treevarlst <- treevarlst
    returnlst$tsumvarlst <- tsumvarlst
  }

  ################################  SEED VARIABLES  ##################################
  if (isseed) {

    ## Variables from FS_NIMS_RMRS.SEEDLING
    seedvarlst <- c("PLT_CN", "SUBP", "CONDID", "SPCD", "SPGRPCD", "TOTAGE", "TPA_UNADJ")

    ## SEED summary variables
    ssumvarlst <- c("TREECOUNT", "TREECOUNT_CALC")

    ## Variables to convert from NA to 0
#    seednavars <- c(ssumvarlst, "TOTAGE", "STOCKING")

    returnlst$seedvarlst <- seedvarlst
    returnlst$ssumvarlst <- ssumvarlst
  }

  ############################  UNDERSTORY VEG VARIABLES  ############################
  if (isveg) {    ## FS_NIMS_FIADB_RMRS or FS_FIADB

    ## Variables from P2VEG_SUBPLOT_SPP
    vsubpsppvarlst <- c("PLT_CN", "SUBP", "CONDID", "VEG_FLDSPCD", "UNIQUE_SP_NBR",
	"VEG_SPCD", "GROWTH_HABIT_CD", "LAYER", "COVER_PCT")

    ### Variables from P2VEG_SUBP_STRUCTURE
    vsubpstrvarlst <- c("PLT_CN", "SUBP", "CONDID", "GROWTH_HABIT_CD", "LAYER",
	"COVER_PCT")

    ### Variables from INVASIVE_SUBPLOT_SPP
    invsubpvarlst <- c("PLT_CN", "SUBP", "CONDID", "VEG_FLDSPCD", "UNIQUE_SP_NBR",
	"VEG_SPCD", "COVER_PCT")


    returnlst$vsubpsppvarlst <- vsubpsppvarlst
    returnlst$vsubpstrvarlst <- vsubpstrvarlst
    returnlst$invsubpvarlst <- invsubpvarlst
  }

  ############################  UNDERSTORY VEG VARIABLES  ############################
  if (issubp) {    ## FS_NIMS_FIADB_RMRS or FS_FIADB

    ## Variables from SUBP
    subpvarlst <- c("PLT_CN", "SUBP", "SUBP_STATUS_CD", "NF_SUBP_STATUS_CD",
		"NF_SUBP_NONSAMPLE_REASN_CD", "P2VEG_SUBP_STATUS_CD",
		"P2VEG_SUBP_NONSAMPLE_REASN_CD", "INVASIVE_SUBP_STATUS_CD",
		"INVASIVE_NONSAMPLE_REASN_CD")

    if (regionVars && regionVarsRS == "RMRS") {
      subpvarlst <- c(subpvarlst, c('GROUND_TRAN_PTS_BARE_RMRS',
		'GROUND_TRAN_PTS_CRYP_RMRS', 'GROUND_TRAN_PTS_DEV_RMRS',
		'GROUND_TRAN_PTS_LICHEN_RMRS', 'GROUND_TRAN_PTS_LITTER_RMRS',
		'GROUND_TRAN_PTS_MOSS_RMRS', 'GROUND_TRAN_PTS_NOTSAMP_RMRS',
		'GROUND_TRAN_PTS_OTHER_RMRS', 'GROUND_TRAN_PTS_PEIS_RMRS',
		'GROUND_TRAN_PTS_ROAD_RMRS', 'GROUND_TRAN_PTS_ROCK_RMRS',
		'GROUND_TRAN_PTS_TRIS_RMRS', 'GROUND_TRAN_PTS_VEG_RMRS',
		'GROUND_TRAN_PTS_WATER_RMRS', 'GROUND_TRAN_PTS_WOOD_RMRS',
		'PREV_STATUSCD_RMRS', 'ROOTSEVCD_RMRS'))
    }

    ## Variables from SUBP_COND
    subpcvarlst <- c("PLT_CN", "SUBP", "CONDID", "MICRCOND_PROP",
		"SUBPCOND_PROP", "MACRCOND_PROP")

    returnlst$subpvarlst <- subpvarlst
    returnlst$subpcvarlst <- subpcvarlst
  }

  ###############################  GRM VARIABLES  ############################
  if (isgrm) {    ## FS_NIMS_FIADB_RMRS or FS_FIADB

    ## Variables from TREE_GRM_COMPONENT
    grmvarlst <- c("PLT_CN", "TRE_CN", "DIA_BEGIN", "DIA_MIDPT", "DIA_END",
	"SUBP_COMPONENT_AL_FOREST", "SUBP_SUBPTYP_GRM_AL_FOREST", 
	"SUBP_TPAGROW_UNADJ_AL_FOREST", "SUBP_TPAMORT_UNADJ_AL_FOREST")
    
    ## Set to NULL for now
    grmbeginvarlst <- NULL

    ### Variables from TREE_GRM_MIDPT
    grmmidptvarlst <- c("TRE_CN", "VOLCFSND", "VOLCFNET", "VOLCSNET", "VOLBFNET",
	"DRYBIO_BG", "DRYBIO_AG", "DRYBIO_WDLD_SPP", "DRYBIO_SAPLING",
 	"DRYBIO_STUMP", "DRYBIO_BOLE", "DRYBIO_SAWLOG", "DRYBIO_TOP")


    returnlst$grmvarlst <- grmvarlst
    returnlst$grmbeginvarlst <- grmbeginvarlst
    returnlst$grmmidptvarlst <- grmmidptvarlst
  }

  ##############################  DOWN WOODY MATERIAL  ###############################
  if (isdwm) {

    ## Variables from COND_DWM_CALC
    dwmvarlst <- c("PLT_CN", "CONDID", "CONDPROP_CWD", "CONDPROP_FWD_SM", "CONDPROP_FWD_MD",
	"CONDPROP_FWD_LG", "CONDPROP_DUFF", "CWD_TL_COND", "CWD_TL_UNADJ", "CWD_TL_ADJ",
	"CWD_LPA_COND", "CWD_LPA_UNADJ", "CWD_LPA_ADJ", "CWD_VOLCF_COND", "CWD_VOLCF_UNADJ",
	"CWD_VOLCF_ADJ", "CWD_DRYBIO_COND", "CWD_DRYBIO_UNADJ", "CWD_DRYBIO_ADJ",
	"CWD_CARBON_COND", "CWD_CARBON_UNADJ", "CWD_CARBON_ADJ", "FWD_SM_TL_COND",
	"FWD_SM_TL_UNADJ", "FWD_SM_TL_ADJ", "FWD_SM_CNT_COND", "FWD_SM_VOLCF_COND",
	"FWD_SM_VOLCF_UNADJ", "FWD_SM_VOLCF_ADJ", "FWD_SM_DRYBIO_COND", "FWD_SM_DRYBIO_UNADJ",
	"FWD_SM_DRYBIO_ADJ", "FWD_SM_CARBON_COND", "FWD_SM_CARBON_UNADJ", "FWD_SM_CARBON_ADJ",
	"FWD_MD_TL_COND", "FWD_MD_TL_UNADJ", "FWD_MD_TL_ADJ", "FWD_MD_CNT_COND",
	"FWD_MD_VOLCF_COND", "FWD_MD_VOLCF_UNADJ", "FWD_MD_VOLCF_ADJ", "FWD_MD_DRYBIO_COND",
	"FWD_MD_DRYBIO_UNADJ", "FWD_MD_DRYBIO_ADJ", "FWD_MD_CARBON_COND", "FWD_MD_CARBON_UNADJ",
	"FWD_MD_CARBON_ADJ", "FWD_LG_TL_COND", "FWD_LG_TL_UNADJ", "FWD_LG_TL_ADJ",
	"FWD_LG_CNT_COND", "FWD_LG_VOLCF_COND", "FWD_LG_VOLCF_UNADJ", "FWD_LG_VOLCF_ADJ",
	"FWD_LG_DRYBIO_COND", "FWD_LG_DRYBIO_UNADJ", "FWD_LG_DRYBIO_ADJ", "FWD_LG_CARBON_COND",
	"FWD_LG_CARBON_UNADJ", "FWD_LG_CARBON_ADJ", "PILE_SAMPLE_AREA_COND",
	"PILE_SAMPLE_AREA_UNADJ", "PILE_SAMPLE_AREA_ADJ", "PILE_VOLCF_COND",
	"PILE_VOLCF_UNADJ", "PILE_VOLCF_ADJ", "PILE_DRYBIO_COND", "PILE_DRYBIO_UNADJ",
	"PILE_DRYBIO_ADJ", "PILE_CARBON_COND", "PILE_CARBON_UNADJ", "PILE_CARBON_ADJ",
	"FUEL_DEPTH", "FUEL_BIOMASS", "FUEL_CARBON", "DUFF_DEPTH", "DUFF_BIOMASS",
	"DUFF_CARBON", "LITTER_DEPTH", "LITTER_BIOMASS", "LITTER_CARBON", "DUFF_TC_COND",
	"DUFF_TC_UNADJ", "DUFF_TC_ADJ", "AVG_WOOD_DENSITY")

    returnlst$dwmvarlst <- dwmvarlst
  }

  return(returnlst)
}


#' @rdname internal_desc
#' @export
DBgetfn <- function(tab, invtype, outfn.pre, stabbrlst=NULL, evalid=NULL, qry=FALSE,
	othertxt=NULL, outfn.date=FALSE, addslash=FALSE, ext="csv", outfolder=NULL,
	overwrite=FALSE) {

  invtypenm <- substr(invtype, 1, 3)

  fn <- ifelse(addslash, "/", "")
  if (!is.null(outfn.pre) && outfn.pre != "") fn <- paste0(fn, outfn.pre, "_")
  fn <- paste0(fn, tab, "_", invtypenm)
  if (!is.null(stabbrlst))
    fn <- paste0(fn, "_", paste(stabbrlst, collapse=""))
  if (!is.null(evalid) && length(evalid) > 0 && length(stabbrlst == 1)) {
    if (length((unique(unlist(evalid)))) == 1) {
      if (inherits(evalid, what = "list")) {
        evalid <- evalid[[1]][1]
      } else {
        evalid <- evalid[1]
      }
      fn <- paste0(fn, "_eval", paste0("20", substr(evalid, nchar(evalid)-3, nchar(evalid)-2)))
    }
  }

  if (!is.null(othertxt)) fn <- paste0(fn, "_", othertxt)
  if (qry) fn <- paste0(fn, "_", "qry")

  if (outfn.date)
    fn <- paste0(fn, "_", format(Sys.time(), "%Y%m%d"))

  if (!overwrite)
    fn <- fileexistsnm(outfolder, fn, ext)

  path.fn <- paste0(outfolder, "/", fn, paste0(".", ext))
  return(path.fn)
}


#' @rdname internal_desc
#' @export
getspconddat <- function(cond=NULL, ACTUALcond=NULL, cuniqueid="PLT_CN", condid1=FALSE,
	ACI=FALSE){

  ## NOTE: The shapefile is generated from coordinates specified from the variable
  ##	'spcoords' which will identify what type of coordinates to use ("ACTUAL", "PUBLIC").
  ##	The shapefile will be in GEOGRAPHIC projection with NAD83 datum.
  ##	The attributes of the shapefile will include all the plot-level variables and some
  ##	condition-level variables (depending on whether they were selected by the user).
  ##
  ## The following criteria is used to determine which condition is included with plots.
  ## (1) MIN COND_STATUS_CD; (2) MAX CONDPROP_UNADJ; (3)MAX CRCOV;
  ##  		(4)MIN STDSZCD; (5)MIN CONDID
  ## If no plot or condition data are included, returns NULL
  #####################################################################################
  CONDMETHOD <- NULL
  cndvarlst <- c(cuniqueid, "CONDPROP_UNADJ", "CONDID", "COND_STATUS_CD", "OWNCD", "OWNGRPCD",
		"FORTYPCD", "FLDTYPCD", "FORTYPCDCALC", "FORTYPGRP", "STDSZCD", "FLDSZCD", "STDAGE",
		"ADFORCD", "RESERVCD", "DSTRBCD1", "DSTRBYR1", "ASPECT", "SLOPE", "LIVE_CANOPY_CVR_PCT")
  if (ACI) cndvarlst <- c(cndvarlst, "NF_COND_STATUS_CD")

  if (!any(cndvarlst %in% names(cond))) {
    message("no condition variables in cond")
    return(NULL)
  }

  cndvars <- names(cond)[which(names(cond) %in% cndvarlst)]
  conddt <- cond[, cndvars, with=FALSE]

  if (!is.null(ACTUALcond)) {
    acndvarlst <- c("ACTUAL_OWNCD", "ACTUAL_FORINDCD")
    conddt <- merge(conddt, ACTUALcond[, c(cuniqueid, acndvarlst), with=FALSE])
  }

  if (!condid1) {
    cndorddf <- data.frame(
		cndordvar=c("PLT_CN", "COND_STATUS_CD", "CONDPROP_UNADJ",
		"LIVE_CANOPY_CVR_PCT", "STDSZCD", "CONDID"),
		cndord=c(1,1,-1,-1,1,1), stringsAsFactors=FALSE)
    cndordvars <- cndorddf$cndordvar[cndorddf$cndordvar %in% names(cond)]
    cndord <- cndorddf$cndord[cndorddf$cndordvar %in% cndordvars]

    ## Order rows of conddt
    data.table::setorderv(conddt, cndordvars, cndord)
    onecond <- conddt[, .SD[1], "PLT_CN"]

    onecond[, CONDMETHOD := "CS_CP_CC_SZ_C1"]
    #onecond[is.na(onecond)] <- 0
    
    message("\n", "CONDMETHOD added to attribute table: ", "\n",
        "Describes which condition-level variables were used to select 1 condition per plot", "\n",
        "  CS - the minimum condition status, emphasizing forested conditions", "\n",
        "  CP - the highest proportion", "\n",
        "  CC - the greatest live percent canopy cover", "\n",
        "  SZ - the largest stand size class", "\n",
        "  C1 - the minimum CONDID", "\n")

  } else {  ## if condid1 = TRUE

    onecond <- conddt[conddt$CONDID == 1, ]
    onecond[, CONDMETHOD := "CONDID1"]
  }
  data.table::setkeyv(onecond, "PLT_CN")

  return(onecond)
}


#' @rdname internal_desc
#' @export
getpfromqry <- function(dsn=NULL, evalid=NULL, plotCur=TRUE, pjoinid=NULL, 
	varCur="MEASYEAR", Endyr=NULL, invyrs=NULL, allyrs=FALSE, SCHEMA.=NULL,
	subcycle99=NULL, designcd1=FALSE, intensity1=NULL, popSURVEY=FALSE, chk=FALSE,
	syntax="sql", plotnm="plot", ppsanm="pop_plot_stratum_assgn", ppsaid="PLT_CN",
	surveynm="survey") {
  ## DESCRIPTION: gets from statement for database query
  ## syntax - ('sql', 'R')

  ## set global variables
  #where.qry <- ""

  if (!is.null(dsn)) {
    dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE)
    tablst <- DBI::dbListTables(dbconn)
    SCHEMA.=NULL
  } else {
    chk <- FALSE
  }

  if (is.null(pjoinid)) {
    pjoinid <- "CN"
  }

  if (!is.null(evalid)) {
    if (chk) {
      ppsanm <- pcheck.varchar(ppsanm, varnm="pop_plot_stratum_assgn",
			checklst=tablst, stopifnull=TRUE)

      evalidlst.qry <- paste("select distinct EVALID from ", ppsanm, "order by EVALID")
      evalidlst <- DBI::dbGetQuery(dbconn, evalidlst.qry)[[1]]
      if (!all(evalid %in% evalidlst)) stop("invalid evalid")
    }
    pfromqry <- paste0(SCHEMA., ppsanm, " ppsa JOIN ",
			SCHEMA., plotnm, " p ON (p.", pjoinid, " = ppsa.", ppsaid, ")")
    if (!is.null(joinfromqry)) {
      pfromqry <- paste0(pfromqry, 
		" JOIN ", SCHEMA., jointable, " ON (p.", pjoinid, " = ", joinalias, ".", joinid, ")") 
    }
    return(pfromqry)
  }

  ## CHeck for plot table in dsn
  if (chk) {
    pltnm <- pcheck.varchar("plot", varnm="plot", checklst=tablst, stopifnull=TRUE)
    pltflds <- DBI::dbListFields(dbconn, pltnm)

    if (!all(c(varCur, "PLOT_STATUS_CD") %in% pltflds))
      stop(paste("must include", toString(c(varCur, "PLOT_STATUS_CD")), "in plot table"))
  }

  if (plotCur) {
    where.qry <- "PLOT_STATUS_CD != 3"
    if (!is.null(subcycle99) && subcycle99) {
      subcycle.filter <- "SUBCYCLE <> 99"
      if (syntax == 'R') gsub("<>", "!=", subcycle.filter)
      if (where.qry == "") {
        where.qry <- subcycle.filter
      } else {
        where.qry <- paste(paste(where.qry, subcycle.filter, sep=" and "))
      }
    }
 
    if (!is.null(intensity1) && intensity1) {
      intensity1.filter <- "INTENSITY = '1'"
      if (syntax == 'R') gsub("=", "==", intensity1.filter)
      if (where.qry == "") {
        where.qry <- intensity1.filter
      } else {
        where.qry <- paste(paste(where.qry, intensity1.filter, sep=" and "))
      }
    }

    if (!is.null(designcd1) && designcd1) {
      designcd1.filter <- "DESIGNCD = 1"
      if (syntax == 'R') gsub("=", "==", designcd1.filter)
      if (where.qry == "") {
        where.qry <- designcd1.filter
      } else {
        where.qry <- paste(paste(where.qry, designcd1.filter, sep=" and "))
      }
    }

    ## Check Endyr
    if (!is.null(Endyr)) {
      #if (!is.numeric(Endyr)) stop("Endyr must be numeric year")
      if (chk) {
        yrlst.qry <- paste("select distinct", varCur, "from", pltnm, "order by INVYR")
        pltyrs <- DBI::dbGetQuery(dbconn, yrlst.qry)

        if (Endyr <= min(pltyrs, na.rm=TRUE))
          stop(Endyr, " is less than minimum year in dataset")
      }
      Endyr.filter <- paste(varCur, "<=", Endyr)
      if (where.qry == "") {
        where.qry <- Endyr.filter
      } else {
        where.qry <- paste(paste(where.qry, Endyr.filter, sep=" and "))
      }
    }

    if (!is.null(where.qry) && any(where.qry != ""))
      where.qry <- paste(" where", where.qry)

    ## create pfromqry
#    if (varCur != "INVYR") {
#      pfromqry <- paste0(SCHEMA., "plot p
#		INNER JOIN
#		(select statecd, unitcd, countycd, plot, max(", varCur, ") maxyr,
#			max(invyr) invyr
#		from ", SCHEMA., "plot", where.qry,
#		" group by statecd, unitcd, countycd, plot) pp
#		ON p.statecd = pp.statecd and
#			p.unitcd = pp.unitcd and
#				p.countycd = pp.countycd and
#					p.plot = pp.plot and p.", varCur,
#						" = pp.maxyr and p.invyr = pp.invyr")
#    } else {

      pfromqry <- paste0(SCHEMA., plotnm, " p
		INNER JOIN
		(select statecd, unitcd, countycd, plot, max(", varCur, ") maxyr
		from ", SCHEMA., plotnm, where.qry,
		" group by statecd, unitcd, countycd, plot) pp
		ON p.statecd = pp.statecd and
			p.unitcd = pp.unitcd and
				p.countycd = pp.countycd and
					p.plot = pp.plot and p.", varCur, " = pp.maxyr")
#    }

    if (popSURVEY) {
      pfromqry <- paste0(pfromqry, " JOIN ", SCHEMA., surveynm,
		" survey on (survey.CN = p.SRV_CN and survey.ANN_INVENTORY = 'Y')")
    }

  } else if (allyrs) {
    pfromqry <- paste0(SCHEMA., plotnm, " p")

  } else if (!is.null(invyrs)) {

    if (chk) {
      invyrlst.qry <- paste("select distinct INVYRS from", pltnm, "order by INVYR")
      pltyrs <- DBI::dbGetQuery(dbconn, invyrlst.qry)

      invyrs.miss <- invyrs[which(!invyrs %in% pltyrs)]
      message("invyrs not in dataset: ", paste(invyrs.miss, collapse=", "))
      if (length(invyrs.miss) == length(invyrs)) stop("")
      invyrs <- invyrs[!invyrs %in% invyrs.miss]
    }
    pfromqry <- paste0(SCHEMA., "plot p")

  } else {
    message("using all plots in database")
    pfromqry <- paste0(SCHEMA., plotnm, " p")
  }
   return(pfromqry)
}


#' @rdname internal_desc
#' @export
getEvalid.ppsa <- function(ppsa, states=NULL, evalAll=FALSE, evalCur=FALSE,
		evalEndyr=NULL, evalType="VOL") {
  ## DESCRIPTION: gets evalid from POP_PLOT_STRATUM_ASSGN table
  ## ARGUMENTS:
  ## chk - Logical. If TRUE, checks if data tables and variables exist

  ## set global variables
  Endyr=EVALID=evaltyp=STATECD=INVYR <- NULL


  ## create state filter
  if (!is.null(states)) {
    stcd <- pcheck.states(states, statereturn="VALUE")
  } else {
    states <- sort(unique(ppsa$STATECD))
  }
  stfilter <- getfilter("STATECD", stcd, syntax='sql')

  eval.qry <- paste("select distinct STATECD, EVALID, max(INVYR) INVYR
			from ppsa
			where", stfilter, "group by STATECD, EVALID",
			"order by STATECD, EVALID")
  evaldt <- data.table::setDT(sqldf::sqldf(eval.qry))


  ## Create lookup for evalType
  evalCode <- c("00","01","01")
  names(evalCode) <- c("ALL", "CURR", "VOL")

  if (is.null(evalType)) {
    evalType <- "00"
  } else if (!all(evalType %in% names(evalCode))) {
    stop("evalType is invalid... must be: ", toString(names(evalCode)))
  }

  ## Get code for evalType
  evalTypecd <- unique(evalCode[which(names(evalCode) %in% evalType)])


  ## add endyr and evaltyp columns to dataframe
  evaldt[, Endyr := substr(EVALID, nchar(EVALID) - 3, nchar(EVALID)-2)]
  evaldt[, evaltyp := substr(EVALID, nchar(EVALID)-1, nchar(EVALID))]

  ## add endyr and evaltyp columns to dataframe
  evaldt[, Endyr := substr(EVALID, nchar(EVALID) - 3, nchar(EVALID)-2)]
  evaldt[, evaltyp := substr(EVALID, nchar(EVALID)-1, nchar(EVALID))]
  if (!all(evalTypecd %in% unique(evaldt$evaltyp))) {
    evaldttyp <- sort(unique(evaldt$evaltyp))
    notype <- evalTypecd[!evalTypecd %in% evaldttyp]
    if (length(notype) > 0) {
      stop(notype, " not in database")
    } else {
      stop("invalid evalType... must be in following list: ", toString(evaldttyp))
    }
  }
  evaldt <- evaldt[evaltyp %in% evalTypecd, ]

  if (evalAll) {
    evalidlist <- sort(unique(evaldt$EVALID))
#  } else if (evalCur) {
#
  } else {
    if (!is.null(evalEndyr)) {
      if (!is.numeric(evalEndyr))  stop("evalEndyr must be numeric yyyy")
      if (nchar(evalEndyr) != 4) stop("evalEndyr must be numeric yyyy")
      yr <- substr(evalEndyr, 3, 4)
      Endyr.max <- evaldt[evaldt[Endyr <= yr, .I[which.max(Endyr)], by="STATECD"]$V1]

    } else {
      data.table::setorder(evaldt, -INVYR, -Endyr)
      Endyr.max <- evaldt[evaldt[, .I[1], by="STATECD"]$V1]

    }
    evalidlist <- Endyr.max[["EVALID"]]
  }
  return(evalidlist)
}


#' @rdname internal_desc
#' @export
getPlotCur <- function(pltx, Endyr=NULL, varCur="MEASYEAR", Endyr.filter=NULL,
	designcd1=TRUE) {
  ## DESCRIPTION: get plots with most current measurement before endyr (if not null)

  ## Set global variables
  pltf <- NULL

  ## Set data.table
  if (!"data.table" %in% class(pltx))
    pltx <- data.table::setDT(pltx)


  ## Get unique identifier for plot location in data table
  if ("PLOT_ID" %in% names(pltx)) {
    uniqueloc <- "PLOT_ID"
  } else {
    uniqueloc <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
    uniqueloc <- uniqueloc[which(uniqueloc %in% names(pltx))]
  }

  ## Check varCur
  if (!varCur %in% names(pltx)) stop(varCur, " not in pltx")

  ## Remove nonsampled plots (PLOT_STATUS_CD == 3)
  if ("PLOT_STATUS_CD" %in% names(pltx)) {
    pltx <- pltx[pltx$PLOT_STATUS_CD < 3,]
  }

  ## Keep only plots where DESIGNCD = 1
  if (designcd1)
    pltx <- pltx[pltx$DESIGNCD == 1,]


  ## Check Endyr
  if (!is.null(Endyr) && is.numeric(Endyr) && Endyr > min(as.numeric(pltx[[varCur]]), na.rm=TRUE)) {
    endyr.filter <- paste(varCur, "<", Endyr)

    if (!is.null(Endyr.filter)) {
      npltx <- nrow(pltx)
      xfilter <- Endyr.filter
      xfilter <- check.logic(pltx, xfilter, filternm="Endyr.filter")
      pltf <- subset(pltx, eval(parse(text = xfilter)))
      if (nrow(pltf) == 0) {
        stop("filter removed all records")
      }

      if (nrow(pltf) == 0) {
        pltf <- NULL
      } else {
        maxyrf <- pltf[eval(parse(text=endyr.filter)), max(get(varCur)), by=uniqueloc]
        data.table::setnames(maxyrf, c(uniqueloc, "maxyr"))
        data.table::setkeyv(maxyrf, c(uniqueloc, "maxyr"))
        data.table::setkeyv(pltf, c(uniqueloc, varCur))
        plotCurf <- pltf[maxyrf]

        xfilter <- paste0("!", Endyr.filter)
        xfilter <- check.logic(pltx, xfilter, filternm="Endyr.filter")
        pltx <- subset(pltx, eval(parse(text = xfilter)))
        if (nrow(pltx) == 0) {
          stop("filter removed all records")
        }
        if ((nrow(pltx) + nrow(pltf)) != npltx) stop("invalid Endyr.filter")
      }
    }
  }

  maxyr <- pltx[, max(get(varCur)), by=uniqueloc]
  data.table::setnames(maxyr, c(uniqueloc, "maxyr"))
  data.table::setkeyv(maxyr, c(uniqueloc, "maxyr"))
  data.table::setkeyv(pltx, c(uniqueloc, varCur))
  plotCur <- pltx[maxyr]


  if (!is.null(pltf)) {
    plotCur <- data.table::rbindlist(list(plotCur, plotCurf))
  }
  return(plotCur)
}


#' @rdname internal_desc
#' @export
gui_filterdf <- function(df, byname=TRUE) {
  ## DESCRIPTION: get filter from a data frame
  ## df - data frame to filter
  ## byname - logical. if TRUE, and variable in ref_codes,
  ## 			gets names from ref_codes
  filterlst <- names(df)

  addfilter <- "yes"
  xfilter <- {}
  while (addfilter != "none") {
    filtervar <- utils::select.list(c("NONE", sort(filterlst)),
      	title=paste("Filter variable"), multiple=FALSE)
    if (filtervar == "") stop("")
    if (filtervar == "NONE") {
      break
    }
    filterval <- sort(unique(df[[filtervar]]))
    if (filtervar %in% c("ELEV", "CRCOVPCT_RMRS", "CRCOVPCT_LIVEMISS_RMRS",
	"CRCOVPCT_LIVE_RMRS", "LIVE_CANOPY_CVR_PCT",
 	"LIVE_MISSING_CANOPY_CVR_PCT") || length(filterval) > 20) {
      ## MINIMUM VALUE
      filtercd_min <- utils::select.list(as.character(filterval),
		title=paste("Select MIN", filtervar), multiple=FALSE)
      if (filtercd_min == "") {
        stop("")
      }
      filterdbmax <- filterval[as.numeric(filterval) >= as.numeric(filtercd_min)]
      ## MAXIMUM VALUE
      filtercd_max <- utils::select.list(as.character(filterdbmax),
			title=paste("Select MAX", filtervar), multiple=FALSE)
      if (filtercd_max == "") {
        stop("")
      }
      xfilter <- paste0(xfilter, paste("(", filtervar, ">=", filtercd_min,
			"and", filtervar, "<=", filtercd_max, ")"))
      byname <- FALSE
    } else {
      if (byname) {
        ref_codes <- ref_codes
        if (filtervar %in% unique(ref_codes$VARIABLE)) {
          filtervalnm <- sort(unique(ref_codes[ref_codes$VARIABLE == filtervar &
	  	ref_codes$VALUE %in% filterval, "MEANING"]))
        } else {
          message("name not in ref_codes... use code to filter")
          filtervalnm <- filterval
          byname <- FALSE
        }
      } else {
        filtervalnm <- filterval
      }

      filter.sel <- utils::select.list(filtervalnm,
			title="Select filter(s)", multiple=TRUE)
      if (length(filter.sel) == 0) {
        stop("")
      }
      if (byname) {
        filter.sel <- unique(ref_codes[ref_codes$VARIABLE == filtervar &
	  	ref_codes$MEANING %in% filter.sel, "VALUE"])
      }

      xfilter <- paste0(xfilter, getfilter(filtervar, filter.sel))

      addfilter <- utils::select.list(c("none", "and", "or"),
	 title=paste("add another filter?"), multiple=FALSE)
      if (addfilter == "") {
        stop("")
      } else {
        filterlst <- filterlst[!filterlst %in% filtervar]
        if (addfilter == "and") {
          xfilter <- paste(xfilter, "& ")
        } else if (addfilter == "or") {
          xfilter <- paste(xfilter, "| ")
        }
      }
    }
  }
  return(xfilter)
}


#' @rdname internal_desc
#' @export
DBgetbyids <- function(dbconn, ids, layernm, layerid="PLT_CN") {
  ## DESCRIPTION: gets data from database from ids (e.g., CN)
  qry <- paste0("select * from ", layernm, " where ",
		layerid, " in(", addcommas(ids, quotes=TRUE), ")")
  rs <- DBI::dbSendQuery(dbconn, qry)
  dat <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  return(dat)
}


#' @rdname internal_desc
#' @export
DBcreateSQLite <- function(SQLitefn=NULL, gpkg=FALSE, dbconnopen=FALSE, 
	outfolder=NULL, outfn.pre=NULL, outfn.date=FALSE, overwrite=FALSE, 
	returnpath=TRUE, stopifnull=FALSE) {
  ## DESCRIPTION: 
  ## Test SQLite connection (SQLite or Geopackage database)
  ## ARGUMENTS:
  ## SQLitefn - String. SQLite filename (*.sqlite or *.gpkg)
  ## dbconnopen - Logical. If TRUE, keep connection to database open
  ## gpkg	- Logical. If TRUE, geopackage database.

  ## Check gpkg
  dbext <- ifelse(gpkg, ".gpkg", ".db")

  ## Check filename
  SQLitePath <- checkfilenm(SQLitefn, outfolder, stopifnull=stopifnull)

  if (is.null(SQLitePath) && is.null(SQLitefn)) {
    SQLitefn <- "data"
  }

  if (!is.null(outfn.pre)) {
    SQLitefn <- paste(outfn.pre, SQLitefn, sep="_")
  }
  
  if (any(is.na(getext(SQLitefn))) || any(getext(SQLitefn) == "NA")) {
    SQLitefn <- paste0(SQLitefn, dbext)
  }
  if (!dir.exists(dirname(SQLitefn))) {
    stop("invalid directory path") 
  }
  SQLitepath <- getoutfn(SQLitefn, outfn.date=outfn.date, 
		outfolder=outfolder, overwrite=overwrite, ext="sqlite")

  ## Overwrite file
  if (file.exists(SQLitepath)) {
    if (overwrite) {
      file.remove(SQLitepath) 
      message("overwriting database... ", SQLitepath)
    } else {
      sqlconn <- DBtestSQLite(SQLitefn=SQLitefn, gpkg=gpkg, dbconnopen=dbconnopen,
		showlist=FALSE)
      if (dbconnopen) return(sqlconn)    
    }
  } else {
    ## Connect to database
    message("creating new SQLite database... ")
    message(SQLitepath)
    sqlconn <- DBI::dbConnect(RSQLite::SQLite(), SQLitepath, loadable.extensions = TRUE)

    if (dbconnopen) {
      return(sqlconn)
    } else {
      DBI::dbDisconnect(sqlconn)
      if (returnpath) {
        return(normalizePath(SQLitepath))
      } else {
        return(basename(SQLitepath))
      }
    }
  }

  if (returnpath) {
    return(SQLitepath)
  } else {
    return(basename(SQLitepath))
  }
}

#' @rdname internal_desc
#' @export
changeclass <- function(tab, noIDate=TRUE) {
  ## Description: changes class of columns if integer64 or IDate
  ## 	if integer64 - if CN, PLT_CN, or PLT_CN, change to character 
  ##	if integer64 - if not CN, PLT_CN, or PLT_CN, change to numeric 
  ##	if IDate - change to character

  isdt <- TRUE
  if (!"data.table" %in% class(tab)) {
    tab <- setDT(tab) 
    isdt <- FALSE
  }
  tabclass <- unlist(lapply(tab, class))
 
  if (any(tabclass == "integer64")) { 
    int64vars <- names(tabclass)[tabclass == "integer64"]
    int64vars.CN <- int64vars[int64vars %in% c("CN", "PLT_CN", "PREV_PLT_CN")]
    int64vars.notCN <- int64vars[!int64vars %in% int64vars.CN]

    if (length(int64vars.CN) > 0) {
      tab[, (int64vars) := lapply(.SD, as.character), .SDcols=int64vars]
    } 
    if (length(int64vars.notCN) > 0) {
      tab[, (int64vars) := lapply(.SD, as.numeric), .SDcols=int64vars]
    } 
  }

  if (noIDate) {
    cols <- names(tab)[unlist(lapply(tab, function(x) any(class(x) == "IDate")))]
    if (length(cols) > 0) {
      tab[, (cols) := lapply(.SD, as.character), .SDcols=cols]
      if ("MODIFIED_DATE" %in% names(tab) && is.logical(tab$MODIFIED_DATE)) {
        tab$MODIFIED_DATE <- as.character(tab$MODIFIED_DATE)
      }
    } 
  } else {
    if ("MODIFIED_DATE" %in% names(tab) && is.logical(tab$MODIFIED_DATE)) {
      tab$MODIFIED_DATE <- as.IDate(tab$MODIFIED_DATE)
    }
  }

  if (isdt) {
    tab <- setDF(tab)
  }
  return(tab)
}

#' @rdname internal_desc
#' @export
customEvalchk <- function(states, measCur = TRUE, measEndyr = NULL, 
		measEndyr.filter = NULL, allyrs = FALSE, invyrs = NULL, 
		measyrs = NULL, invyrtab = NULL, gui=FALSE) {

  ### Check measCur
  ###########################################################
  measCur <- pcheck.logical(measCur, varnm="measCur", 
			title="Current measyear?", first="YES", gui=gui)

  ### Check measEndyr
  ###########################################################
  measEndyr.filter <- NULL
  if (!is.null(measEndyr)) {
    minyr <- min(invyrtab$INVYR)
    if (!is.numeric(measEndyr) || measEndyr < minyr)
      stop("measEndyr must be yyyy format and greater than minimum inventory year: ", minyr)
    measCur <- TRUE
    measEndyr.filter <- paste0(" and MEASYEAR < ", measEndyr)
  }
  if (measCur) {
    allyrs <- FALSE
  }  

  ### GET allyrs
  ###########################################################
  allyrs <- pcheck.logical(allyrs, varnm="allyrs", 
		title="All years?", first="YES", gui=gui)
  if (allyrs) {
    measCur <- FALSE
    measEndyr=measEndyr.filter <- NULL
  }
 
  ## Check INVYR(S) 
  ###########################################################
  if (!measCur) {
    if ((is.null(invyrs) || length(invyrs) == 0) && 
		(is.null(measyrs) || length(measyrs) == 0)) {
      if (is.null(invyrtab)) {
        return(NULL)
      } 
      invyrs <- sapply(states, function(x) NULL)
      for (state in states) { 
        stabbr <- pcheck.states(state, "ABBR")
        if ("STATENM" %in% names(invyrtab)) {
          stinvyrlst <- sort(invyrtab[invyrtab$STATENM == state, "INVYR"])
        } else if ("STATECD" %in% names(invyrtab)) {
          stcd <- pcheck.states(state, "VALUE")
          stinvyrlst <- sort(invyrtab[invyrtab$STATECD == stcd, "INVYR"])
        }        
        if (allyrs) {
          invyr <- stinvyrlst
        } else {
          if (!gui) {
            return(NULL)
          }
          ## GET INVENTORY YEAR(S) FROM USER
          invyr <- select.list(as.character(stinvyrlst),
                         title = paste("Inventory year(s) -", stabbr), 
                         multiple = TRUE)
          if (length(invyr) == 0) stop("")
        }
        invyrs[[state]] <- as.numeric(invyr)
      }
    } else if (!is.null(invyrs)) {
      if (!is(invyrs, "list")) {
        if (is.vector(invyrs) && is.numeric(invyrs)) {
          invyrs <- list(invyrs)
          if (length(states) == 1) {
            names(invyrs) <- states
          } else {
            warning("using specified invyrs for all states")
            yrs <- invyrs
            invyrs <- sapply(states, function(x) NULL)
            for (st in states) invyrs[st] <- yrs
          } 
        }
      } else if (length(invyrs) != length(states)) {
        stop("check invyrs list.. does not match number of states")
      }
      ## Check inventory years
      for (state in states) {
        stcd <- pcheck.states(state, "VALUE")
        if ("STATENM" %in% names(invyrtab)) {
          stinvyrlst <- sort(invyrtab[invyrtab$STATENM == state, "INVYR"])
        } else if ("STATECD" %in% names(invyrtab)) {
          stinvyrlst <- sort(invyrtab[invyrtab$STATECD == stcd, "INVYR"])
        } else {
          stop("invyrtab is invalid")
        }
        if (!all(invyrs[[state]] %in% stinvyrlst)) {
          invyrs[[state]] <- invyrs[[state]][invyrs[[state]] %in% stinvyrlst]
          missyr <- invyrs[[state]][!invyrs[[state]] %in% stinvyrlst]
          message(state, " missing following inventory years: ", toString(missyr))
        }
      }
    } else if (!is.null(measyrs)) {
      if (!is(measyrs, "list")) {
        if (is.vector(measyrs) && is.numeric(measyrs)) {
          measyrs <- list(measyrs)
          if (length(states) == 1) {
            names(measyrs) <- states
          } else {
            message("using specified invyrs for all states")
            yrs <- measyrs
            measyrs <- sapply(states, function(x) NULL)
            for (st in states) measyrs[st] <- yrs
          } 
        }
      } else if (length(measyrs) != length(states)) {
        stop("check measyrs list.. does not match number of states")
      }
      ## Check inventory years
      for (state in states) {
        stcd <- pcheck.states(state, "VALUE")
        names(invyrtab) <- toupper(names(invyrtab))
        yrnm <- ifelse("INVYR" %in% names(invyrtab), "INVYR", 
			ifelse("MEASYEAR" %in% names(invyrtab), "MEASYEAR", "NONE"))
        if (yrnm == "NONE") {
          stop("invyrtab is invalid")
        }           
 
        if ("STATENM" %in% names(invyrtab)) {
          stinvyrlst <- sort(invyrtab[invyrtab$STATENM == state, yrnm])
        } else if ("STATECD" %in% names(invyrtab)) {
          stinvyrlst <- sort(invyrtab[invyrtab$STATECD == stcd, yrnm])
        } else {
          stop("invyrtab is invalid")
        }
        if (!all(measyrs[[state]] %in% stinvyrlst)) {
          measyrs[[state]] <- measyrs[[state]][measyrs[[state]] %in% stinvyrlst]
          missyr <- measyrs[[state]][!measyrs[[state]] %in% stinvyrlst]
          message(state, " missing following inventory years: ", toString(missyr))
        }
      }
    }
  }

  returnlst <- list(measCur=measCur, measEndyr=measEndyr, 
		measEndyr.filter=measEndyr.filter, allyrs=allyrs, 
		invyrs=invyrs, measyrs=measyrs)
}

