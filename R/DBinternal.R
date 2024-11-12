## DBvars.default	## Set default variable lists for extracting data from database
## DBgetfn			## Gets file name for Plot and Strata files.
## getspconddat
## getpfromqry      ## Get pfromqry for extracting data
## getpwithqry       ## Get with query for extracting data
## getplotCur
## getEvalid.ppsa   ## Get Evalid from pop_plot_stratum_assgn
## gui_filterdf		## Get filter from a data frame
## DBgetbyids		## Gets data from database from ids
## changeclass
## customEvalchk
## addftypgrp       ## Appends forest type group codes to table
## checkidx         ## Check index for a table in database
## createidx        ## Create index for a table in database
## dbclassify       ## Add a column with classified values 


#' @rdname internal_desc
#' @export
DBvars.default <- function(istree=FALSE, isseed=FALSE, isveg=FALSE, isgrm=FALSE, 
	isdwm=FALSE, issubp=FALSE, regionVars=FALSE, plotgeom=FALSE, regionVarsRS="RMRS",
	iscwd=FALSE) {

  ## Set global variables
  treevarlst=tsumvarlst=seedvarlst=ssumvarlst=vsubpsppvarlst=vsubpstrvarlst=
	dwmlst=pgeomvarlst=cwdlst <- NULL

  ## DESCRIPTION: Set default variables for FIA plot data extraction

  ########################################################################
  ##  DEFINE VARIABLES
  ########################################################################
  ref_units <- FIESTAutils::ref_units
  #volvars <- c("VOLCFNET", "VOLCFGRS", "VOLBFNET", "VOLBFGRS", "VOLCSNET", "VOLCSGRS")
  volvars <- ref_units$VARIABLE[grepl("VOL", ref_units$VARIABLE)]
  #growvars <- c("GROWCFGS", "GROWCFAL", "FGROWCFGS", "FGROWCFAL")
  #mortvars <- c("MORTCFGS", "MORTCFAL", "FMORTCFGS", "FMORTCFAL")
  #remvars <- c("REMVCFGS", "REMVCFAL", "FREMVCFGS", "FREMVCFAL")
  #tpavars <- c("TPA_UNADJ", "TPAGROW_UNADJ", "TPAMORT_UNADJ", "TPAREMV_UNADJ")
  tpavars <- "TPA_UNADJ"
#  biovars <- c("DRYBIO_BOLE", "DRYBIO_STUMP", "DRYBIO_TOP", "DRYBIO_SAPLING",
#		"DRYBIO_WDLD_SPP", "DRYBIO_BG", "DRYBIO_AG")
  biovars <- ref_units$VARIABLE[grepl("DRYBIO", ref_units$VARIABLE)]
  carbonvars <- ref_units$VARIABLE[grepl("CARBON", ref_units$VARIABLE)]


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
	"CARBON_DOWN_DEAD", "CARBON_LITTER", "CARBON_SOIL_ORG",
	"CARBON_UNDERSTORY_AG", "CARBON_UNDERSTORY_BG", "NF_COND_STATUS_CD",
	"NF_COND_NONSAMPLE_REASN_CD","LAND_COVER_CLASS_CD")
  ## Note: CARBON_STANDING_DEAD was removed Oct 2023 Version 9.1

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
		"FVS_VARIANT", "FVS_LOC_CD", "FVS_REGION", "FVS_FOREST", "FVS_DISTRICT",
		"ROADLESSCD")
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
    tsumvarlst <- c(volvars, tpavars, biovars, carbonvars)

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
	"SUBP_TPAGROW_UNADJ_AL_FOREST", "SUBP_TPAMORT_UNADJ_AL_FOREST",
	"MICR_COMPONENT_AL_FOREST", "MICR_SUBPTYP_GRM_AL_FOREST",
	"MICR_TPAGROW_UNADJ_AL_FOREST")
    
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
	"CONDPROP_FWD_LG", "CONDPROP_DUFF", "CWD_TL_COND", "CWD_TL_UNADJ", 
	"CWD_LPA_COND", "CWD_LPA_UNADJ", "CWD_VOLCF_COND", "CWD_VOLCF_UNADJ",
	 "CWD_DRYBIO_COND", "CWD_DRYBIO_UNADJ", "CWD_CARBON_COND", "CWD_CARBON_UNADJ", "FWD_SM_TL_COND",
	"FWD_SM_TL_UNADJ", "FWD_SM_CNT_COND", "FWD_SM_VOLCF_COND",
	"FWD_SM_VOLCF_UNADJ", "FWD_SM_DRYBIO_COND", "FWD_SM_DRYBIO_UNADJ",
	"FWD_SM_CARBON_COND", "FWD_SM_CARBON_UNADJ",
	"FWD_MD_TL_COND", "FWD_MD_TL_UNADJ", "FWD_MD_CNT_COND",
	"FWD_MD_VOLCF_COND", "FWD_MD_VOLCF_UNADJ", "FWD_MD_DRYBIO_COND",
	"FWD_MD_DRYBIO_UNADJ", "FWD_MD_CARBON_COND", "FWD_MD_CARBON_UNADJ",
	"FWD_MD_CARBON_ADJ", "FWD_LG_TL_COND", "FWD_LG_TL_UNADJ", 
	"FWD_LG_CNT_COND", "FWD_LG_VOLCF_COND", "FWD_LG_VOLCF_UNADJ", 
	"FWD_LG_DRYBIO_COND", "FWD_LG_DRYBIO_UNADJ", "FWD_LG_CARBON_COND",
	"FWD_LG_CARBON_UNADJ", "PILE_SAMPLE_AREA_COND",
	"PILE_SAMPLE_AREA_UNADJ", "PILE_SAMPLE_AREA_ADJ", "PILE_VOLCF_COND",
	"PILE_VOLCF_UNADJ", "PILE_DRYBIO_COND", "PILE_DRYBIO_UNADJ",
	"PILE_CARBON_COND", "PILE_CARBON_UNADJ", 
	"FUEL_DEPTH", "FUEL_BIOMASS", "FUEL_CARBON", "DUFF_DEPTH", "DUFF_BIOMASS",
	"DUFF_CARBON", "LITTER_DEPTH", "LITTER_BIOMASS", "LITTER_CARBON", "DUFF_TC_COND",
	"DUFF_TC_UNADJ", "AVG_WOOD_DENSITY")

    returnlst$dwmvarlst <- dwmvarlst
  }
  
  
  if (iscwd) {
    
    ## Variables from DWM_COARSE_WOODY_DEBRIS
    cwdvarlst <- c("PLT_CN", "CONDID", "SPCD", "LPA_UNADJ", 
                    "VOLCF_AC_UNADJ", "DRYBIO_AC_UNADJ", 
                    "DRYBIO_AC_PLOT", "CARBON_AC_UNADJ")
    
    returnlst$cwdvarlst <- cwdvarlst
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
getpfromqry <- function(dsn=NULL, evalid=NULL, plotCur=TRUE, pjoinid,
     varCur="MEASYEAR", Endyr=NULL, invyrs=NULL, allyrs=FALSE, SCHEMA.=NULL,
     subcycle99=NULL, designcd1=FALSE, intensity1=NULL, popSURVEY=FALSE, 
     chk=FALSE, Type="VOL", syntax="sql", plotnm="plot", 
     ppsanm="pop_plot_stratum_assgn", ppsaid="PLT_CN", surveynm="survey", 
     plotobj=NULL, dbconn=NULL, dbconnopen=TRUE) {
  ## DESCRIPTION: gets from statement for database query
  ## syntax - ('sql', 'R')
  ## evalid - Integer. EVALID code defining FIA Evaluation
  ## plotCur - Logical. If TRUE, gets most current plot
  ## pjoinid - String. Name of variable in plot table to join
  ## varCur - String. Name of variable to use for most current plot
  ##            ('MEASYEAR', 'INVYR')
  ## Endyr - Integer. Year to determine most current measurement
  ## invyrs - Integer vector. Inventory years to query
  ## allyrs - Logical. All years in database
  ## SCHEMA. - Oracle schema
  ## subcycle99 - Logical. If TRUE, include plots with subcycle=99
  ## designcd1 - Logical. If TRUE, include only plots with DESIGNCD = 1
  ## intensity1 - Logical. If TRUE, include only plots with INTENSITY = 1
  ## popSURVEY - Logical. If TRUE, include SURVEY table in query
  ## chk - Logical. If TRUE, check for variables 
  ## Type - Logical. Type of query ('All', 'Vol')
  ## syntax - String. SQL or R query syntax ('sql', 'R')
  ## plotnm - String. Name of plot table in database or as R object.
  ## ppsanm - String. Name of plot_pop_stratum_assgn table
  ## ppsaid - String. Name of unique id in ppsa
  ## surveynm - String. Name of survey table 
  ## plotobj - R object. Plot table if exists as R object

  ## set global variables
  #where.qry <- ""

  if (!is.null(dbconn)) {
    tablst <- DBI::dbListTables(dbconn)
    SCHEMA.=NULL
  } else if (!is.null(dsn)) {
    dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE)
    tablst <- DBI::dbListTables(dbconn)
    SCHEMA.=NULL
  } else {
    chk <- FALSE
  }

  if (!is.null(evalid)) {
    if (chk) {
      ppsanm <- pcheck.varchar(ppsanm, varnm="pop_plot_stratum_assgn",
			checklst=tablst, stopifnull=TRUE)

      evalidlst.qry <- paste("select distinct EVALID from ", ppsanm, "order by EVALID")
      evalidlst <- DBI::dbGetQuery(dbconn, evalidlst.qry)[[1]]
      if (!all(evalid %in% evalidlst)) stop("invalid evalid")
    }
    pfromqry <- paste0(SCHEMA., ppsanm, " ppsa \nJOIN ",
			SCHEMA., plotnm, " p ON (ppsa.", ppsaid, " = p.", pjoinid, ")")
    return(pfromqry)
  }

  if (plotCur) {
    if (any(Type == "All")) {
      where.qry <- ""
	} else {
      where.qry <- "PLOT_STATUS_CD != 3"
    }
    if (!is.null(subcycle99) && !subcycle99) {
      subcycle.filter <- "SUBCYCLE <> 99"
      if (syntax == 'R') gsub("<>", "!=", subcycle.filter)
      if (where.qry == "") {
        where.qry <- subcycle.filter
      } else {
        where.qry <- paste(paste(where.qry, subcycle.filter, sep=" and "))
      }
    }
 
    if (!is.null(intensity1) && intensity1) {
      if (!is.null(dsn)) {
        intensitynm <- findnm(DBI::dbListFields(dbconn, plotnm), returnNULL=TRUE)
      } else {
        intensitynm <- findnm("INTENSITY", names(plotobj), returnNULL=TRUE)
      }
 
      if (!is.null(intensitynm)) {
        intensity1.filter <- paste(intensitynm, "= '1'")
        if (syntax == 'R') gsub("=", "==", intensity1.filter)
        if (where.qry == "") {
          where.qry <- intensity1.filter
        } else {
          where.qry <- paste(paste(where.qry, intensity1.filter, sep=" and "))
        }
      } else {
        message("INTENSITY variable is not in data... assuming all INTENSITY=1 plots")
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
        yrlst.qry <- paste("select distinct", varCur, "\nfrom", plotnm, "order by INVYR")
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

    if (!is.null(where.qry) && any(where.qry != "")) {
      where.qry <- paste(" \nWHERE", where.qry)
    }

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

      pfromqry <- paste0(SCHEMA., plotnm, " p ",
		"\nINNER JOIN 
		(SELECT statecd, unitcd, countycd, plot, MAX(", varCur, ") maxyr
		  \nFROM ", SCHEMA., plotnm, 
             where.qry,
		  " \n  GROUP BY statecd, unitcd, countycd, plot) pp
		       ON p.statecd = pp.statecd AND
                         p.unitcd = pp.unitcd AND
                             p.countycd = pp.countycd AND
                                 p.plot = pp.plot AND p.", varCur, " = pp.maxyr")
#    }

    if (popSURVEY) {
      pfromqry <- paste0(pfromqry, " \nJOIN ", SCHEMA., surveynm,
		" survey ON (survey.CN = p.SRV_CN AND survey.ANN_INVENTORY = 'Y')")
    }

  } else if (allyrs) {
    pfromqry <- paste0(SCHEMA., plotnm, " p")

  } else if (!is.null(invyrs)) {

    if (chk) {
      invyrlst.qry <- paste("SELECT DISTINCT invyr FROM", plotnm, "ORDER BY invyr")
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
  
  if (!is.null(dbconn) && !dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
  return(pfromqry)
}



#' @rdname internal_desc
#' @export
getpwithqry <- function(dsn = NULL, evalid = NULL, states = NULL, 
     pjoinid, plotCur = FALSE, varCur="MEASYEAR", Endyr=NULL, invyrs=NULL, 
	   measyears = NULL, allyrs = FALSE, SCHEMA.=NULL, invtype = "ANNUAL",
     subcycle99 = FALSE, designcd1=FALSE, intensity=NULL, popSURVEY=FALSE, 
     chk=FALSE, Type = "VOL", syntax = "sql", plotnm = "plot", 
     ppsanm = "pop_plot_stratum_assgn", ppsaid = "PLT_CN", surveynm = "survey", 
     PLOTdf = NULL, pltflds = NULL, POP_PLOT_STRATUM_ASSGNdf = NULL, 
	   ppsaflds = NULL, pvars = NULL, dbconn = NULL, dbconnopen = TRUE,
	   withqry2 = FALSE) {
  ## DESCRIPTION: gets from statement for database query
  ## syntax - ('sql', 'R')
  ## evalid - Integer. EVALID code defining FIA Evaluation
  ## plotCur - Logical. If TRUE, gets most current plot
  ## pjoinid - String. Name of variable in plot table to join
  ## varCur - String. Name of variable to use for most current plot
  ##            ('MEASYEAR', 'INVYR')
  ## Endyr - Integer. Year to determine most current measurement
  ## invyrs - Integer vector. Inventory years to query
  ## allyrs - Logical. All years in database
  ## SCHEMA. - Oracle schema
  ## subcycle99 - Logical. If TRUE, include plots with subcycle=99
  ## designcd1 - Logical. If TRUE, include only plots with DESIGNCD = 1
  ## intensity - Logical. If TRUE, include only plots with defined intensity values
  ## popSURVEY - Logical. If TRUE, include SURVEY table in query
  ## chk - Logical. If TRUE, check for variables 
  ## Type - Logical. Type of query ('All', 'Vol')
  ## syntax - String. SQL or R query syntax ('sql', 'R')
  ## plotnm - String. Name of plot table in database or as R object.
  ## ppsanm - String. Name of plot_pop_stratum_assgn table
  ## ppsaid - String. Name of unique id in ppsa
  ## surveynm - String. Name of survey table 
  ## PLOTdf - R object. Plot table if exists as R object

  ## set global variables
  where.qry <- ""
  
  ## Get inventory type
  anntype <- ifelse(invtype == "ANNUAL", "Y", "N")

  ## Check database and/or database connection
  if (!is.null(dbconn)) {
    tablst <- DBI::dbListTables(dbconn)
    SCHEMA.=NULL
  } else if (!is.null(dsn)) {
    dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE)
    tablst <- DBI::dbListTables(dbconn)
    SCHEMA.=NULL
  } else if (is.null(pltflds)) {
    chk <- FALSE
  }

  ## Get plot fields
  if (is.null(pltflds)) {
    if (!is.null(plotnm)) {
      if (!is.null(dbconn)) {
        pltflds <- DBI::dbListFields(dbconn, plotnm)
      } else if (!is.null(PLOTdf)) {
	      pltflds <- names(PLOTdf)
	    }
    }
  }
 
  ## Check pjoinid
  if (is.null(pjoinid)) {
	  if (!is.null(pltflds)) {
      pjoinid <- findnm("CN", pltflds, returnNULL=TRUE)	
	    if (is.null(pjoinid)) {
        pjoinid <- findnm("PLT_CN", pltflds, returnNULL=TRUE)	
		    if (is.null(pjoinid)) {
		      pjoinid <- "CN"
		    }
	    }
    } 		
  }
 
  ## Define plot select variables
  if (is.null(pvars)) {
	  selectpvars <- "p.*"
  } else {
	  selectpvars <- toString(paste0("p.", unique(c(pjoinid, pvars))))
  }

  ###################################################################################
  ## GET pfromqry
  ###################################################################################
  if (!is.null(evalid)) {
    subcycle99 <- TRUE
    pfromqry <- paste0("\nFROM ", SCHEMA., ppsanm, " ppsa \nJOIN ",
			SCHEMA., plotnm, " p ON (ppsa.", ppsaid, " = p.", pjoinid, ")")
  } else {
    pfromqry <- paste0("\nFROM ", SCHEMA., plotnm, " p")
 
    if (popSURVEY) {
      pfromqry <- paste0(pfromqry, 
		             "\n  INNER JOIN ", SCHEMA., surveynm, " survey 
		             ON (survey.CN = p.SRV_CN AND survey.ANN_INVENTORY = '", anntype, "')")
    }
  }

  ###################################################################################
  ## GET whereqry
  ###################################################################################
  if (!is.null(evalid)) {
		
    ## Get ppsaflds
    if (is.null(ppsaflds)) {
      if (!is.null(ppsanm)) {
        if (!is.null(dbconn)) {
          ppsaflds <- DBI::dbListFields(dbconn, ppsanm)
		    } else if (!is.null(POP_PLOT_STRATUM_ASSGNdf)) {
		      ppsaflds <- names(POP_PLOT_STRATUM_ASSGNdf)
	      }
      }
    }

	## Get name of EVALID column
	evalidnm <- findnm("EVALID", ppsaflds, returnNULL=TRUE)
	if (is.null(evalidnm)) {
	  evalidnm <- "EVALID"
	}
	## Get whereqry
	where.qry <- paste0(evalidnm, " in(", toString(evalid), ")")
		
  } else if (!is.null(states)) { 
	  statenm <- findnm("STATECD", pltflds, returnNULL=TRUE)
	  if (!is.null(statenm)) {
	    statenm <- "STATECD"
	  }
	  where.qry <- paste0("p.", statenm, " in(", toString(states), ")")
  }

  ## Add plot_status_cd to where statement
  if (any(Type == "All") || !is.null(evalid)) {
    where.qry <- where.qry
  } else {
	  plotstatusnm <- findnm("PLOT_STATUS_CD", pltflds, returnNULL=TRUE)
	  if (is.null(plotstatusnm)) {
	    message("PLOT_STATUS_CD not in data... assuming all sampled plots")
	  } else {
	    plotstatus.qry <- paste0("p.", plotstatusnm, " <> 3")
	    if (syntax == 'R') plotstatus.qry <- gsub("<>", "!=", plotstatus.qry)
	    if (where.qry == "") {
	      where.qry <- plotstatus.qry
	    } else {
        where.qry <- paste0(where.qry, " and ", plotstatus.qry)
	    } 
	  } 
  }
  ## Add subcycle to where statement
  if (!is.null(subcycle99) && !subcycle99) {
	  subcyclenm <- findnm("SUBCYCLE", pltflds, returnNULL=TRUE)
	  if (is.null(subcyclenm)) {
	    message("SUBCYCLE not in data... assuming all SUBCYCLE <> 99")
	  } else {
      subcycle.filter <- paste0("p.", subcyclenm, " <> 99")
      if (syntax == 'R') subcycle.filter <- gsub("<>", "!=", subcycle.filter)
      if (where.qry == "") {
        where.qry <- subcycle.filter
      } else {
        where.qry <- paste(paste(where.qry, subcycle.filter, sep=" and "))
      }
	  }
  }

  ## Add intensity to where statement 
  if (!is.null(intensity)) {
    intensitynm <- findnm("INTENSITY", pltflds, returnNULL=TRUE)
    if (is.null(intensitynm)) {
	  message("INTENSITY variable not in data... assuming all INTENSITY=1")
    } else {
      intensity.filter <- paste0("p.", 
	            getfilter("INTENSITY", intensity, syntax=syntax, quote=TRUE))
      if (where.qry == "") {
        where.qry <- intensity.filter
      } else {
        where.qry <- paste(paste(where.qry, intensity.filter, sep=" and "))
      }
    }
  }
  ## Add designcd to where statement
  if (!is.null(designcd1) && designcd1) {
    designcdnm <- findnm("DESIGNCD", pltflds, returnNULL=TRUE)
    if (is.null(designcdnm)) {
	  message("DESIGNCD variable not in data... assuming all DESIGNCD=1")
	} else {
      designcd1.filter <- "DESIGNCD = 1"
      if (syntax == 'R') designcd1.filter <- gsub("=", "==", designcd1.filter)
      if (where.qry == "") {
        where.qry <- designcd1.filter
      } else {
        where.qry <- paste(paste(where.qry, designcd1.filter, sep=" and "))
      }
    }
  }

  if (plotCur) {
    ## Add an Endyr to where statement
    if (!is.null(Endyr)) {
      #if (!is.numeric(Endyr)) stop("Endyr must be numeric year")
      if (chk) {
        yrlst.qry <- paste("select distinct", varCur, "\nfrom", plotnm, "order by INVYR")
        pltyrs <- DBI::dbGetQuery(dbconn, yrlst.qry)

        if (Endyr <= min(pltyrs, na.rm=TRUE))
          stop(Endyr, " is less than minimum year in dataset")
      }
      Endyr.filter <- paste0("p.", varCur, " <= ", Endyr)
      if (where.qry == "") {
        where.qry <- Endyr.filter
      } else {
        where.qry <- paste(paste(where.qry, Endyr.filter, sep=" and "))
      }
    }

    ## Define group variables
    groupvars <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
    if (!is.null(pltflds)) {
	    pgroupvars <- sapply(groupvars, findnm, pltflds, returnNULL = TRUE)
	    if (any(is.null(pgroupvars))) {
	      missvars <- pgroupvars[is.null(pgroupvars)]
        if (length(missvars) > 1 || missvars != "unitcd") {
		      warning("dataset must include statecd, countycd, and plot")
        }
	    } else {
	      groupvars <- as.vector(pgroupvars)
	    }
    }

    ## Create WITH query
    withqry <- paste0(
	  "WITH ",
	  "\nmaxyear AS",
      "\n (SELECT distinct ", toString(paste0("p.", groupvars)), ", MAX(p.", varCur, ") maxyr  ",
	       pfromqry)
	  
	  if (!is.null(where.qry) || where.qry != "") {
      withqry <- paste0(withqry,  " \n  WHERE ", where.qry)
    }
	
	  withqry <- paste0(withqry,
	      "\n  GROUP BY ", toString(paste0("p.", groupvars)), ")")

	  ## Add an query to get CN values
    if (withqry2) {	
	     withqry2 <- paste0(
       "\np AS",   	
       "\n (SELECT ", selectpvars,
	     "\n  FROM ", SCHEMA., plotnm, " p INNER JOIN maxyear ON(")
	   
	    for (i in 1:length(groupvars)) {
	      gvar <- groupvars[i]
	      withqry2 <- paste0(withqry2, "p.", gvar, " = maxyear.", gvar)	   
	      if (i < length(groupvars)) {
	        withqry2 <- paste0(withqry2, " and ")
	      }
	    }
	    withqry2 <- paste0(withqry2, " and p.", varCur, " = maxyear.maxyr))")
	 
	    withqry <- paste0(withqry, ",", withqry2)
	  }

  } else if (!is.null(invyrs)) {

    if (chk) {
      invyrlst.qry <- paste("SELECT DISTINCT invyr FROM", plotnm, "ORDER BY invyr")
      pltyrs <- DBI::dbGetQuery(dbconn, invyrlst.qry)

      invyrs.miss <- invyrs[which(!invyrs %in% pltyrs)]
      message("invyrs not in dataset: ", paste(invyrs.miss, collapse=", "))
      if (length(invyrs.miss) == length(invyrs)) stop("")
      invyrs <- invyrs[!invyrs %in% invyrs.miss]
    }
	
    ## Create WITH query
    withqry <- paste0(
	    "WITH ",
	    "\np AS",
      "\n (SELECT distinct ", selectpvars,
	    pfromqry)	   
	   
	  ## Add invyrs to where statement 
    invyrnm <- findnm("INVYR", pltflds, returnNULL=TRUE)
    if (is.null(invyrnm)) {
	    message("INVYR variable not in data")
    } else {
      invyr.filter <- paste0("p.", invyrnm, " in(", toString(invyrs), ")")
      if (syntax == 'R') invyr.filter <- gsub("in\\(", "%in% c\\(", invyr.filter)
      if (where.qry == "") {
        where.qry <- invyr.filter
      } else {
        where.qry <- paste(paste(where.qry, invyr.filter, sep=" and "))
      }
    }
	   	  
	  if (!is.null(where.qry) || where.qry != "") {
      withqry <- paste0(withqry,  " \n  WHERE ", where.qry, ")")
    }

  } else if (!is.null(measyears)) {

    if (chk) {
      measyrlst.qry <- paste("SELECT DISTINCT measyear FROM", plotnm, "ORDER BY measyear")
      pltyrs <- DBI::dbGetQuery(dbconn, measyrlst.qry)

      measyr.miss <- measyears[which(!measyears %in% pltyrs)]
      message("invyrs not in dataset: ", paste(invyrs.miss, collapse=", "))
      if (length(measyr.miss) == length(measyears)) stop("")
      measyears <- measyears[!measyears %in% measyr.miss]
    }
	
    ## Create WITH query
    withqry <- paste0(
	             "WITH ",
	             "\np AS",
               "\n (SELECT ", selectpvars,
	             pfromqry)	   
	   
	  ## Add invyrs to where statement 
    measyrnm <- findnm("MEASYEAR", pltflds, returnNULL=TRUE)
    if (is.null(measyrnm)) {
	    message("MEASYEAR variable not in data")
    } else {
      measyr.filter <- paste0("p.", measyrnm, " in(", toString(measyears), ")")
      if (syntax == 'R') measyr.filter <- gsub("in\\(", "%in% c\\(", measyr.filter)
      if (where.qry == "") {
        where.qry <- measyr.filter
      } else {
        where.qry <- paste(paste(where.qry, measyr.filter, sep=" and "))
      }
    }
	   	  
	  if (!is.null(where.qry) || where.qry != "") {
      withqry <- paste0(withqry,  " \n  WHERE ", where.qry, ")")
    }

  } else {
 
    ## Create WITH query
    withqry <- paste0(
	             "WITH ",
	             "\np AS",
               "\n (SELECT distinct ", selectpvars,
	             pfromqry)
  
	  if (!is.null(where.qry) || where.qry != "") {
      withqry <- paste0(withqry,  " \n  WHERE ", where.qry, ")")
    }	  
  }
  
  if (!is.null(dbconn) && !dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
  
  return(withqry)
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
  #rs <- DBI::dbSendQuery(dbconn, qry)
  #dat <- DBI::dbFetch(rs)
  #DBI::dbClearResult(rs)
  dat <- DBI::dbGetQuery(dbconn, qry)
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

  ## Set global variables
  invyrlst=measyrlst <- NULL


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
  if (!measCur || !is.null(invyrs) || !is.null(measyrs)) {

    if (allyrs) {
      return(list(measCur=measCur, allyrs=allyrs, 
                  invyrs=invyrs, measyrs=measyrs))
    }
    if ((is.null(invyrs) || length(invyrs) == 0) && 
		           (is.null(measyrs) || length(measyrs) == 0)) {
      if (is.null(invyrtab)) {
        return(NULL)
      } 
      invyrlst <- sapply(states, function(x) NULL)
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
        } else if (gui) {
          #if (!gui) {
          #  return(NULL)
          #}
          ## GET INVENTORY YEAR(S) FROM USER
          invyr <- select.list(as.character(stinvyrlst),
                         title = paste("Inventory year(s) -", stabbr), 
                         multiple = TRUE)
          if (length(invyr) == 0) stop("")
        } else {
          invyr <- stinvyrlst
          allyrs <- TRUE
        }
        invyrlst[[state]] <- as.numeric(invyr)
      }

    } else if (!is.null(invyrs)) {
      if (!is(invyrs, "list")) {
        if (is.vector(invyrs) && is.numeric(invyrs)) {
          invyrlst <- list(invyrs)
          if (length(states) == 1) {
            names(invyrlst) <- states
          } else {
            message("using specified invyrs for all states")
            yrs <- invyrs
            invyrlst <- sapply(states, function(x) NULL)
            for (st in states) invyrlst[[st]] <- yrs
          } 
        }
      } else if (length(invyrs) != length(states)) {
        stop("check invyrs list.. does not match number of states")
      } else {
        invyrlst <- invyrs
      }
 
      if (!is.null(invyrtab)) {
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
          if (!all(invyrlst[[state]] %in% stinvyrlst)) {
            invyrlst[[state]] <- invyrlst[[state]][invyrs[[state]] %in% stinvyrlst]
            missyr <- invyrlst[[state]][!invyrlst[[state]] %in% stinvyrlst]
            message(state, " missing following inventory years: ", toString(missyr))
          }
        }
      }
    } else if (!is.null(measyrs)) {
      if (!is(measyrs, "list")) {
        if (is.vector(measyrs) && is.numeric(measyrs)) {
          measyrlst <- list(measyrs)
          if (length(states) == 1) {
            names(measyrlst) <- states
          } else {
            message("using specified invyrs for all states")
            yrs <- measyrs
            measyrlst <- sapply(states, function(x) NULL)
            for (st in states) measyrlst[[st]] <- yrs
          } 
        }
      } else if (length(measyrs) != length(states)) {
        stop("check measyrs list.. does not match number of states")
      }

      if (!is.null(invyrtab)) {

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
          if (!all(measyrlst[[state]] %in% stinvyrlst)) {
            measyrlst[[state]] <- measyrlst[[state]][measyrlst[[state]] %in% stinvyrlst]
            missyr <- measyrlst[[state]][!measyrlst[[state]] %in% stinvyrlst]
            message(state, " missing following inventory years: ", toString(missyr))
          }
        }
      }
    }
  }
  returnlst <- list(measCur=measCur, measEndyr=measEndyr, 
		                measEndyr.filter=measEndyr.filter, allyrs=allyrs, 
		                invyrs=invyrs, measyrs=measyrs, 
                    invyrlst=invyrlst, measyrlst=measyrlst)
}



#' @rdname internal_desc
#' @export
checkidx <- function(dbconn, tbl = NULL, index_cols = NULL, 
                     datsource = "sqlite", schema = "FS_FIADB",
					 dbconnopen = TRUE) {
  ## DESCRIPTION: checks table in database
  ## dbconn - open database connection
  ## tbl - one or more tables to check
  ## index_cols - columns included in index to check
  ## datsource - type of database connection ('sqlite', 'oracle')
  ## schema - a schema in a database
  
  ## Set global variables
  tblnm <- NULL
  
  if (is.character(dbconn)) {
    message("must be an open connection to check")
	return(NULL)
  }

  ## Check database connection
  if (is.null(dbconn) || !DBI::dbIsValid(dbconn)) {
    message("dbconn is not valid")
    return(NULL)
  }
  
  ## Get dbconn information
  dbinfo <- DBI::dbGetInfo(dbconn)
  
  ## Check tbl
  ######################################################
  ## Get list of tables
  if (!is.null(tbl)) {
    if (datsource == "sqlite") {
      tablst <- DBI::dbListTables(dbconn)
    } else {
      qry <- paste0(
         "SELECT DISTINCT object_name 
          FROM all_objects
          WHERE object_type = 'TABLE'
		  AND owner = 'FS_FIADB'
		  ORDER BY object_name")
      tablst <- DBI::dbGetQuery(dbconn, qry)[,1]
    }
  
    ## Check tbl in database
    tblnm <- unique(unlist(sapply(tbl, findnm, tablst, returnNULL=TRUE)))
    if (is.null(tblnm)) {
      warning(tbl, " does not exist")
	    message("tables in database: ", toString(tablst))
      return(0)
    }
  }
  
  ## Check tbl
  ######################################################  
  if (datsource == "sqlite") {
    index.qry <- paste0(
      "SELECT name, tbl_name, sql", 
      "\nFROM sqlite_master",
			"\nWHERE type = 'index'")
    if (!is.null(tblnm)) {
	    index.qry <- paste(
	      index.qry,
	      "\n   AND tbl_name in(", addcommas(tblnm, quotes=TRUE), ")",
			  "\nORDER BY tbl_name")
	  }
  } else {  ## datsource != 'sqlite'
    index.qry <- paste0("SELECT table_name, index_name", 
	              "\nFROM all_indexes")
				  
	if (!is.null(schema)) {
	  if (!is.character(schema) || length(schema) > 1) {
	    message("schema must be a character vector of length = 1")
		  return(0)
	  }
	  index.qry <- paste0(index.qry,
				  "\nWHERE table_owner = '", schema, "'")
	
	  indices <- tryCatch(
	      DBI::dbGetQuery(dbconn, index.qry),
				error=function(e) {
				    warning(e)
  			    return(NULL)})
	  if (is.null(indices)) {
	    schema.qry <- "SELECT DISTINCT table_owner FROM all_indexes"
	    schemalst <-  DBI::dbGetQuery(dbconn, schema.qry)[,1]
		  message("schema is invalid: ", toString(schemalst))
	  } else {
	    indices <- setDT(indices)
	  }
	}  
				  
  if (!is.null(tblnm)) {
	  if (!is.null(schema)) {
	    index.qry <- paste0(
	      index.qry,
	      "\n   AND table_name in(", addcommas(tblnm, quotes=TRUE), ")",
				"\nORDER BY table_name")
	  } else {
	  	index.qry <- paste0(
	  	  index.qry,
	      "\nWHERE table_name in(", addcommas(tblnm, quotes=TRUE), ")",
				"\nORDER BY table_name")
	    }
	  }			 
  }
  #message(index.qry)
     
  indices <- DBI::dbGetQuery(dbconn, index.qry)
  if (is.null(tblnm)) {
    return(indices)
  } 
  
  if (nrow(indices) > 0) {
    message("checking indices in database...")
  }

  if (datsource == "sqlite") {
  
  	getcols <- function(idx) {
        split1 <- strsplit(idx, "\\(")[[1]][2]
        split2 <- strsplit(split1, "\\)")[[1]][1]
	    return(split2)
		}
	  indices$cols <- sapply(indices$sql, getcols)

    if (!is.null(index_cols)) {
      index_test <- data.frame(sapply(index_cols, 
             function(x) grepl(x, indices$cols, ignore.case=TRUE)))
      index_test$TEST <- apply(index_test, 1, all)
      if (!any(index_test$TEST)) {
        message("index does not exist")
        return(NULL)
      } else {
        return(unique(indices$tbl_name[index_test$TEST]))
      } 
    } else {
      return(indices[, c("tbl_name", "sql", "cols")])
	  }
  } else {
    return(indices)
  }
  if (!dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
}

#' @rdname internal_desc
#' @export
createidx <- function(dbconn, schema=NULL, tbl, index_cols, unique=FALSE, dbconnopen=TRUE) {
  ## DESCRIPTION: create index

  SCHEMA. <- ""
  if (is.character(dbconn)) {
    message("must check an open connection")
	  return(NULL)
  }

  if (!DBI::dbIsValid(dbconn)) {
    message("dbconn is not valid")
    return(NULL)
  }
  if (!is.null(schema)) {
    SCHEMA. <- paste0(schema, ".")
  }
  
  flds <- DBI::dbListTables(dbconn)

  tblnm <- findnm(tbl, flds, returnNULL=TRUE)
  indxnm <- paste0(tblnm, "_", paste(tolower(index_cols), collapse="_"), "_idx")
 
  if (unique) {
    idxsql <- paste0("create unique index ", indxnm, " ON ", tbl,
				"(", paste(index_cols, collapse=","), ")")
  } else {
    idxsql <- paste0("create index ", indxnm, " ON ", tbl,
				"(", paste(index_cols, collapse=","), ")")
  }

  test <- tryCatch(
            DBI::dbExecute(dbconn, idxsql),
		    error=function(err) {
				message(err, "\n")
		    } )
  if (!is.null(test)) {
    message(sub("create", "creating", idxsql))
  }
  
  if (!dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
}


#' @rdname internal_desc
#' @export
dbclassify <- function(dbconn, 
                       tabnm, 
                       classcol,
                       cutbreaks,
                       cutlabels,
                       classnm = NULL, 
                       overwrite = TRUE, 
                       NAto0 = FALSE,
                       dbconnopen = TRUE,
					   quiet = FALSE) {
  ## DESCRIPTION: Add a column with classified values to tabnm
  ## dbconn - open database connection
  ## tabnm - String. table in database with column to classify
  ## classcol - String. column in table to classify
  ## cutbreaks - Numeric vector. Breakpoints in classcol for classifying
  ## cutlabels - String vector. 

  if (!DBI::dbIsValid(dbconn)) {
    message("invalid database dbconnection") 
    return(NULL)
  }

  ## Get list of tables in dbconn
  tablst <- DBI::dbListTables(dbconn)
  if (length(tablst) == 0) {
    message("no tables in database")
    return(NULL)
  }
     
  ## Get list of fields in tabnm
  tab <- chkdbtab(tablst, tabnm)
  tabflds <- DBI::dbListFields(dbconn, tab)  

  ## Check classcol
  if (!classcol %in% tabflds) {
    message(classcol, " not in ", tabnm)
    return(NULL)
  }

  ## Check classnm
  if (is.null(classnm)) {
    classnm <- paste0(classcol, "CL")
  } 
   
  ## If duplicate column...
  if (classnm %in% tabflds) {
    if (overwrite) {
      dropcol.qry <- paste("ALTER TABLE", tabnm, 
                          "DROP COLUMN", classnm)
      DBI::dbExecute(dbconn, dropcol.qry)
    } else {   
      classnm <- checknm(classnm, tabflds)
    }
  }

  ## Add empty column to table
  datatype <- class(cutlabels)
  if (datatype == "numeric") {
    if (all(!grepl("\\.", cutlabels))) {
      type <- "integer"
    } else {
      type <- "numeric"
    }  
  } else {
    type <- paste0("varchar(", max(nchar(cutlabels)) + 3, ")")
  } 

  alter.qry <- paste("ALTER TABLE", tabnm, 
                     "ADD", classnm, type)
  DBI::dbExecute(dbconn, alter.qry)

  ## Build classify query
  classify1.qry <- paste("UPDATE", tabnm, "SET", classnm, "= (CASE")

  classify2.qry <- {}
  if (NAto0) {
    classify2.qry <- paste("   WHEN", classcol, "is null THEN '0'")
  }
  
  for (i in 1:(length(cutbreaks)-1)) {    
    classify2.qry <- paste(classify2.qry, 
                         "\n  WHEN", classcol, ">=", cutbreaks[i], "AND", 
                              classcol, "<", cutbreaks[i+1], "THEN", 
                              paste0("'", cutlabels[i], "'"))
  }
  classify.qry <- paste0(classify1.qry, classify2.qry, " END)")
  if (!quiet) {
    message(classify.qry)
  }
  
  nbrrows <- DBI::dbExecute(dbconn, classify.qry)
  message("updated ", nbrrows, " rows")

  tabcnt.qry <- paste("select", classnm, ", count(*) COUNT from", tabnm,
                       "group by", classnm)
  tabcnt <- DBI::dbGetQuery(dbconn, tabcnt.qry)
  if (!quiet) {
    messagedf(tabcnt)
  }

  if (!dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
  return(0)
}


#' @rdname internal_desc
#' @export
dbclass <- function(dbconn, 
                    tabnm, 
                    classcol,
                    classvals,
                    classlabels,
                    classnm = NULL, 
                    overwrite = TRUE, 
                    NAto0 = FALSE,
                    dbconnopen = TRUE,
					quiet = FALSE) {
  ## DESCRIPTION: Add a column with classified values to tabnm
  ## dbconn - open database connection
  ## tabnm - String. table in database with column to classify
  ## classcol - String. column in table to classify
  ## cutbreaks - Numeric vector. Breakpoints in classcol for classifying
  ## cutlabels - String vector. 

  if (!DBI::dbIsValid(dbconn)) {
    message("invalid database dbconnection") 
    return(NULL)
  }

  ## Get list of tables in dbconn
  tablst <- DBI::dbListTables(dbconn)
  if (length(tablst) == 0) {
    message("no tables in database")
    return(NULL)
  }
     
  ## Get list of fields in tabnm
  tab <- chkdbtab(tablst, tabnm)
  tabflds <- DBI::dbListFields(dbconn, tab)  

  ## Check classcol
  if (!classcol %in% tabflds) {
    message(classcol, " not in ", tabnm)
    return(NULL)
  }

  ## Check classnm
  if (is.null(classnm)) {
    classnm <- paste0(classcol, "CL")
  } 
   
  ## If duplicate column...
  if (classnm %in% tabflds) {
    if (overwrite) {
      dropcol.qry <- paste("ALTER TABLE", tabnm, 
                          "DROP COLUMN", classnm)
      DBI::dbExecute(dbconn, dropcol.qry)
    } else {   
      classnm <- checknm(classnm, tabflds)
    }
  }

  ## Add empty column to table
  datatype <- class(classlabels)
  if (datatype == "numeric") {
    if (all(!grepl("\\.", classlabels))) {
      type <- "integer"
    } else {
      type <- "numeric"
    }  
  } else {
    type <- paste0("varchar(", max(nchar(classlabels)) + 3, ")")
  } 

  alter.qry <- paste("ALTER TABLE", tabnm, 
                     "ADD", classnm, type)
  DBI::dbExecute(dbconn, alter.qry)

  ## Build classify query
  classify1.qry <- paste("UPDATE", tabnm, "SET", classnm, "= (CASE")

  classify2.qry <- {}
  if (NAto0) {
    classify2.qry <- paste("   WHEN", classcol, "is null THEN '0'")
  }
  
  for (i in 1:(length(classvals)-1)) {    
    classify2.qry <- paste(classify2.qry, 
                         "\n  WHEN", classcol, "=", classvals[i], "THEN", 
                              paste0("'", classlabels[i], "'"))
  }
  classify.qry <- paste0(classify1.qry, classify2.qry, " END)")
  if (!quiet) {
    message(classify.qry)
  }
  
  nbrrows <- DBI::dbExecute(dbconn, classify.qry)
  message("updated ", nbrrows, " rows")

  tabcnt.qry <- paste("select", classnm, ", count(*) COUNT from", tabnm,
                       "group by", classnm)
  tabcnt <- DBI::dbGetQuery(dbconn, tabcnt.qry)
  if (!quiet) {
    messagedf(tabcnt)
  }

  if (!dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
  return(0)
}



#' @rdname internal_desc
#' @export
getwithqry <- function(dsn = NULL, evalid = NULL, states = NULL, 
     pjoinid, plotCur = FALSE, varCur="MEASYEAR", Endyr=NULL, invyrs=NULL, 
	 measyears = NULL, allyrs = FALSE, SCHEMA.=NULL, invtype = "ANNUAL",
     subcycle99 = FALSE, designcd1=FALSE, intensity1=FALSE, popSURVEY=FALSE, 
     chk=FALSE, Type = "VOL", syntax = "sql", plotnm = "plot", 
     ppsanm = "pop_plot_stratum_assgn", ppsaid = "PLT_CN", surveynm = "survey", 
     PLOTdf = NULL, pltflds = NULL, POP_PLOT_STRATUM_ASSGNdf = NULL, 
	 ppsaflds = NULL, pvars = NULL, dbconn = NULL, dbconnopen = TRUE,
	 withqry2 = FALSE) {
  ## DESCRIPTION: gets from statement for database query
  ## syntax - ('sql', 'R')
  ## evalid - Integer. EVALID code defining FIA Evaluation
  ## plotCur - Logical. If TRUE, gets most current plot
  ## pjoinid - String. Name of variable in plot table to join
  ## varCur - String. Name of variable to use for most current plot
  ##            ('MEASYEAR', 'INVYR')
  ## Endyr - Integer. Year to determine most current measurement
  ## invyrs - Integer vector. Inventory years to query
  ## allyrs - Logical. All years in database
  ## SCHEMA. - Oracle schema
  ## subcycle99 - Logical. If TRUE, include plots with subcycle=99
  ## designcd1 - Logical. If TRUE, include only plots with DESIGNCD = 1
  ## intensity - Logical. If TRUE, include only plots with defined intensity values
  ## popSURVEY - Logical. If TRUE, include SURVEY table in query
  ## chk - Logical. If TRUE, check for variables 
  ## Type - Logical. Type of query ('All', 'Vol')
  ## syntax - String. SQL or R query syntax ('sql', 'R')
  ## plotnm - String. Name of plot table in database or as R object.
  ## ppsanm - String. Name of plot_pop_stratum_assgn table
  ## ppsaid - String. Name of unique id in ppsa
  ## surveynm - String. Name of survey table 
  ## PLOTdf - R object. Plot table if exists as R object

  ## set global variables
  where.qry <- ""
  intensity <- NULL
  if (intensity1) intensity <- 1
  
  ## Get inventory type
  anntype <- ifelse(invtype == "ANNUAL", "Y", "N")

  ## Check database and/or database connection
  if (!is.null(dbconn)) {
    tablst <- DBI::dbListTables(dbconn)
    SCHEMA.=NULL
  } else if (!is.null(dsn)) {
    dbconn <- DBtestSQLite(dsn, dbconnopen=TRUE)
    tablst <- DBI::dbListTables(dbconn)
    SCHEMA.=NULL
  } else if (is.null(pltflds)) {
    chk <- FALSE
  }

  ## Get plot fields
  if (is.null(pltflds)) {
    if (!is.null(plotnm)) {
      if (!is.null(dbconn)) {
        pltflds <- DBI::dbListFields(dbconn, plotnm)
      } else if (!is.null(PLOTdf)) {
	    pltflds <- names(PLOTdf)
	  }
    }
  }
 
  ## Check pjoinid
  if (is.null(pjoinid)) {
	if (!is.null(pltflds)) {
      pjoinid <- findnm("CN", pltflds, returnNULL=TRUE)	
	  if (is.null(pjoinid)) {
        pjoinid <- findnm("PLT_CN", pltflds, returnNULL=TRUE)	
		if (is.null(pjoinid)) {
		  pjoinid <- "CN"
		}
	  }
    } 		
  }
 
  ## Define plot select variables
  if (is.null(pvars)) {
	selectpvars <- "p.*"
  } else {
	selectpvars <- toString(paste0("p.", unique(c(pjoinid, pvars))))
  }

  ###################################################################################
  ## GET pfromqry
  ###################################################################################
  if (!is.null(evalid)) {
    subcycle99 <- TRUE
    pfromqry <- paste0("\nFROM ", SCHEMA., ppsanm, " ppsa \nJOIN ",
			SCHEMA., plotnm, " p ON (ppsa.", ppsaid, " = p.", pjoinid, ")")
  } else {
    pfromqry <- paste0("\nFROM ", SCHEMA., plotnm, " p")
 
    if (popSURVEY) {
      pfromqry <- paste0(pfromqry, 
		"\n  INNER JOIN ", SCHEMA., surveynm, " survey 
		ON (survey.CN = p.SRV_CN AND survey.ANN_INVENTORY = '", anntype, "')")
    }
  }

  ###################################################################################
  ## GET whereqry
  ###################################################################################
  if (!is.null(evalid)) {
		
    ## Get ppsaflds
    if (is.null(ppsaflds)) {
      if (!is.null(ppsanm)) {
        if (!is.null(dbconn)) {
          ppsaflds <- DBI::dbListFields(dbconn, ppsanm)
		} else if (!is.null(POP_PLOT_STRATUM_ASSGNdf)) {
		  ppsaflds <- names(POP_PLOT_STRATUM_ASSGNdf)
	    }
      }
    }

	## Get name of EVALID column
	evalidnm <- findnm("EVALID", ppsaflds, returnNULL=TRUE)
	if (is.null(evalidnm)) {
	  evalidnm <- "EVALID"
	}
	## Get whereqry
	where.qry <- paste0(evalidnm, " in(", toString(evalid), ")")
		
  } else if (!is.null(states)) { 
	statenm <- findnm("STATECD", pltflds, returnNULL=TRUE)
	if (!is.null(statenm)) {
	  statenm <- "STATECD"
	}
	where.qry <- paste0("p.", statenm, " in(", toString(states), ")")
  }

  ## Add plot_status_cd to where statement
  if (any(Type == "All") || !is.null(evalid)) {
    where.qry <- where.qry
  } else {
	plotstatusnm <- findnm("PLOT_STATUS_CD", pltflds, returnNULL=TRUE)
	if (is.null(plotstatusnm)) {
	  message("PLOT_STATUS_CD not in data... assuming all sampled plots")
	} else {
	  plotstatus.qry <- paste0("p.", plotstatusnm, " <> 3")
	  if (syntax == 'R') plotstatus.qry <- gsub("<>", "!=", plotstatus.qry)
	  if (where.qry == "") {
	    where.qry <- plotstatus.qry
	  } else {
        where.qry <- paste0(where.qry, " and ", plotstatus.qry)
	  } 
	} 
  }
  ## Add subcycle to where statement
  if (!is.null(subcycle99) && !subcycle99) {
	subcyclenm <- findnm("SUBCYCLE", pltflds, returnNULL=TRUE)
	if (is.null(subcyclenm)) {
	  message("SUBCYCLE not in data... assuming all SUBCYCLE <> 99")
	} else {
      subcycle.filter <- paste0("p.", subcyclenm, " <> 99")
      if (syntax == 'R') subcycle.filter <- gsub("<>", "!=", subcycle.filter)
      if (where.qry == "") {
        where.qry <- subcycle.filter
      } else {
        where.qry <- paste(paste(where.qry, subcycle.filter, sep=" and "))
      }
	}
  }
  ## Add intensity to where statement 
  if (!is.null(intensity)) {
    intensitynm <- findnm("INTENSITY", pltflds, returnNULL=TRUE)
    if (is.null(intensitynm)) {
	  message("INTENSITY variable not in data... assuming all INTENSITY=1")
    } else {
      intensity.filter <- paste0("p.", 
	            getfilter("INTENSITY", intensity, syntax=syntax, quote=TRUE))
      if (where.qry == "") {
        where.qry <- intensity.filter
      } else {
        where.qry <- paste(paste(where.qry, intensity.filter, sep=" and "))
      }
    }
  }
  ## Add designcd to where statement
  if (!is.null(designcd1) && designcd1) {
    designcdnm <- findnm("DESIGNCD", pltflds, returnNULL=TRUE)
    if (is.null(designcdnm)) {
	  message("DESIGNCD variable not in data... assuming all DESIGNCD=1")
	} else {
      designcd1.filter <- "DESIGNCD = 1"
      if (syntax == 'R') designcd1.filter <- gsub("=", "==", designcd1.filter)
      if (where.qry == "") {
        where.qry <- designcd1.filter
      } else {
        where.qry <- paste(paste(where.qry, designcd1.filter, sep=" and "))
      }
    }
  }

  if (plotCur) {
    ## Add an Endyr to where statement
    if (!is.null(Endyr)) {
      #if (!is.numeric(Endyr)) stop("Endyr must be numeric year")
      if (chk) {
        yrlst.qry <- paste("select distinct", varCur, "\nfrom", plotnm, "order by INVYR")
        pltyrs <- DBI::dbGetQuery(dbconn, yrlst.qry)

        if (Endyr <= min(pltyrs, na.rm=TRUE))
          stop(Endyr, " is less than minimum year in dataset")
      }
      Endyr.filter <- paste0("p.", varCur, " <= ", Endyr)
      if (where.qry == "") {
        where.qry <- Endyr.filter
      } else {
        where.qry <- paste(paste(where.qry, Endyr.filter, sep=" and "))
      }
    }

    ## Define group variables
    groupvars <- c("STATECD", "UNITCD", "COUNTYCD", "PLOT")
    if (!is.null(pltflds)) {
	  pgroupvars <- sapply(groupvars, findnm, pltflds, returnNULL = TRUE)
	  if (any(is.null(pgroupvars))) {
	    missvars <- pgroupvars[is.null(pgroupvars)]
        if (length(missvars) > 1 || missvars != "unitcd") {
		  warning("dataset must include statecd, countycd, and plot")
        }
	  } else {
	    groupvars <- as.vector(pgroupvars)
	  }
    }

    ## Create WITH query
    withqry <- paste0(
	  "WITH ",
	  "\nmaxyear AS",
      "\n (SELECT ", toString(paste0("p.", groupvars)), ", MAX(p.", varCur, ") maxyr  ",
	  pfromqry)

	  
	if (!is.null(where.qry) || where.qry != "") {
      withqry <- paste0(withqry,  " \n  WHERE ", where.qry)
    }
	
	withqry <- paste0(withqry,
	      "\n  GROUP BY ", toString(paste0("p.", groupvars)), ")")
	
	## Add an query to get CN values
    if (withqry2) {	
	  withqry2 <- paste0(
       "\np AS",   	
       "\n (SELECT ", selectpvars,
	   "\n  FROM ", SCHEMA., plotnm, " p INNER JOIN maxyear ON(")
	   
	  for (i in 1:length(groupvars)) {
	    gvar <- groupvars[i]
	    withqry2 <- paste0(withqry2, "p.", gvar, " = maxyear.", gvar)	   
	    if (i < length(groupvars)) {
	      withqry2 <- paste0(withqry2, " and ")
	    }
	  }
	  withqry2 <- paste0(withqry2, " and p.", varCur, " = maxyear.maxyr))")
	 
	  withqry <- paste0(withqry, ",", withqry2)
	}

  } else if (!is.null(invyrs)) {

    if (chk) {
      invyrlst.qry <- paste("SELECT DISTINCT invyr FROM", plotnm, "ORDER BY invyr")
      pltyrs <- DBI::dbGetQuery(dbconn, invyrlst.qry)

      invyrs.miss <- invyrs[which(!invyrs %in% pltyrs)]
      message("invyrs not in dataset: ", paste(invyrs.miss, collapse=", "))
      if (length(invyrs.miss) == length(invyrs)) stop("")
      invyrs <- invyrs[!invyrs %in% invyrs.miss]
    }
	
    ## Create WITH query
    withqry <- paste0(
	  "WITH ",
	  "\np AS",
      "\n (SELECT ", selectpvars,
	  pfromqry)	   
	   
	## Add invyrs to where statement 
    invyrnm <- findnm("INVYR", pltflds, returnNULL=TRUE)
    if (is.null(invyrnm)) {
	  message("INVYR variable not in data")
    } else {
      invyr.filter <- paste0("p.", invyrnm, " in(", toString(invyrs), ")")
      if (syntax == 'R') invyr.filter <- gsub("in\\(", "%in% c\\(", invyr.filter)
      if (where.qry == "") {
        where.qry <- invyr.filter
      } else {
        where.qry <- paste(paste(where.qry, invyr.filter, sep=" and "))
      }
    }
	   	  
	if (!is.null(where.qry) || where.qry != "") {
      withqry <- paste0(withqry,  " \n  WHERE ", where.qry, ")")
    }

  } else if (!is.null(measyears)) {

    if (chk) {
      measyrlst.qry <- paste("SELECT DISTINCT measyear FROM", plotnm, "ORDER BY measyear")
      pltyrs <- DBI::dbGetQuery(dbconn, measyrlst.qry)

      measyr.miss <- measyears[which(!measyears %in% pltyrs)]
      message("invyrs not in dataset: ", paste(invyrs.miss, collapse=", "))
      if (length(measyr.miss) == length(measyears)) stop("")
      measyears <- measyears[!measyears %in% measyr.miss]
    }
	
    ## Create WITH query
    withqry <- paste0(
	  "WITH ",
	  "\np AS",
      "\n (SELECT ", selectpvars,
	  pfromqry)	   
	   
	## Add invyrs to where statement 
    measyrnm <- findnm("MEASYEAR", pltflds, returnNULL=TRUE)
    if (is.null(measyrnm)) {
	  message("MEASYEAR variable not in data")
    } else {
      measyr.filter <- paste0("p.", measyrnm, " in(", toString(measyears), ")")
      if (syntax == 'R') measyr.filter <- gsub("in\\(", "%in% c\\(", measyr.filter)
      if (where.qry == "") {
        where.qry <- measyr.filter
      } else {
        where.qry <- paste(paste(where.qry, measyr.filter, sep=" and "))
      }
    }
	   	  
	if (!is.null(where.qry) || where.qry != "") {
      withqry <- paste0(withqry,  " \n  WHERE ", where.qry, ")")
    }

  } else {
  
    ## Create WITH query
    withqry <- paste0(
	  "WITH ",
	  "\np AS",
      "\n (SELECT ", selectpvars,
	  pfromqry)
  
	if (!is.null(where.qry) || where.qry != "") {
      withqry <- paste0(withqry,  " \n  WHERE ", where.qry, ")")
    }	  
  }
  
  if (!is.null(dbconn) && !dbconnopen) {
    DBI::dbDisconnect(dbconn)
  }
  
  return(withqry)
}
