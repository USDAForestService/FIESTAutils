#' @rdname internal_desc
#' @export
subpsampCHNG <- function(sccm, condu, pltu, data_dsn = NULL,
                     subpuniqueid = "PLT_CN", subpid = "SUBP", 
                     ACI = FALSE, pfromqry = NULL, whereqry = NULL) {

  ## DESCRIPTION: Summarize sampled subplot condition proportions.

  ## Set global variables
  gui <- FALSE

  ## Check sccm
  sccm <- pcheck.table(sccm, tab_dsn=data_dsn, tabnm="sccm", 
			caption="SUBP_COND_CHNG_MTRX table?", stopifnull = TRUE)

  ## Check condu
  condu <- pcheck.table(condu, tab_dsn=data_dsn, tabnm="condu",
			caption="Cond table?")

  ## Check pltu
  pltu <- pcheck.table(pltu, tab_dsn=data_dsn, tabnm="pltu",
			caption="Plot table?", stopifnull = TRUE)


  ############################################################
  ## Assemble from query
  ############################################################
  SCHEMA. <- NULL
  palias <- "p"
  dbqueries <- list()


  if (is.null(pfromqry)) {
    pfromqry <- paste0(SCHEMA., "pltu ", palias)   
  }
  pchgfromqry <- paste0(pfromqry, 
	" JOIN ", SCHEMA., pltux, 
	" pplot ON(pplot.", puniqueid, " = ", palias, ".PREV_PLT_CN)")


  ############################################################
  ## Assemble where clause
  ############################################################

  ## Get where statement for plot change query
  pchgwhere <- paste0(palias, ".REMPER > 0") 

  if (is.null(whereqry)) {
    pchgwhereqry <- paste("WHERE", pchgwhere)
  } else {
    pchgwhereqry <- paste(whereqry, "and", pchgwhere)
  }

  ############################################################
  ## pltu query
  ############################################################

  ## Get default variables for plot
  pltvars <- DBvars.default()$pltvarlst
  pltvars <- pltvars[pltvars %in% names(pltux)]


  ## Build query for plot change
  pltuqry <- paste("select distinct", 
				toString(paste0(palias, ".", pltvars)), 
					"from", pchgfromqry, pchgwhereqry,
			  "UNION select distinct", 
				toString(paste0("pplot.", pltvars)), 
					"from", pchgfromqry, pchgwhereqry)
  dbqueries$pltu <- pltuqry


  ############################################################
  ## condu query
  ############################################################

  ## Get default variables for cond
  condvars <-  DBvars.default()$condvarlst
  condvars <- condvars[condvars %in% names(condux)]


  ## Sum condition proportions by subplot
  #########################################################
  if (!is.null(subplotx)) {

    ## Generate query for calculating subplot-level adjustments
    sumcprop.qry <- paste0(
        "SELECT c.PLT_CN, c.CONDID, c.COND_STATUS_CD, subc.SUBP,
              SUM(subc.SUBPCOND_PROP)  AS SUBPPROP_UNADJ,
              SUM(subc.MICRCOND_PROP) AS MICRPROP_UNADJ,
              SUM(subc.MACRCOND_PROP) AS MACRPROP_UNADJ,
              SUM(CASE WHEN subc.MACRCOND_PROP IS NOT NULL
                       THEN subc.MACRCOND_PROP
                       ELSE subc.SUBPCOND_PROP end) AS CONDPROP_UNADJ
         FROM condx c
              JOIN subplotx subp ON (subp.PLT_CN = c.PLT_CN)
              JOIN subp_condx subc ON (subc.PLT_CN = c.PLT_CN
                                          and subc.CONDID = c.CONDID
                                          and subc.SUBP = subp.SUBP)
         WHERE ", subpwhereqry,
             " AND ", cwhereqry, 
             " GROUP BY c.PLT_CN, c.CONDID, subc.SUBP, c.COND_STATUS_CD")

  } else {

    ## Generate query for calculating subplot-level adjustments
    sumcprop.qry <- paste0(
        "SELECT c.PLT_CN, c.CONDID, c.COND_STATUS_CD, subc.SUBP,
              SUM(subc.SUBPCOND_PROP) AS SUBPPROP_UNADJ,
              SUM(subc.MICRCOND_PROP) AS MICRPROP_UNADJ,
              SUM(subc.MACRCOND_PROP) AS MACRPROP_UNADJ,
              SUM(CASE WHEN subc.MACRCOND_PROP IS NOT NULL
                       THEN subc.MACRCOND_PROP
                       ELSE subc.SUBPCOND_PROP end) AS CONDPROP_UNADJ
         FROM condx c
              JOIN subp_condx subc ON (subc.PLT_CN = c.PLT_CN
                                          and subc.CONDID = c.CONDID)
         WHERE ", cwhereqry, 
             " GROUP BY c.PLT_CN, c.CONDID, subc.SUBP, c.COND_STATUS_CD")
  }

  subpcx <- data.table(sqldf::sqldf(sumcprop.qry))
  return(subpcx)
}


