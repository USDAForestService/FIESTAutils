#' @rdname internal_desc
#' @export
subpsamp <- function(cond, subp_cond, subplot, data_dsn = NULL,
                     subpuniqueid = "PLT_CN", subpid = "SUBP", 
                     ACI = FALSE) {

  ## DESCRIPTION: Summarize sampled subplot condition proportions.

  ## Set global variables
  gui <- FALSE

  ## Check cond
  condx <- pcheck.table(cond, tab_dsn=data_dsn, tabnm="cond", 
			caption="Condition table?")

  ## Check subplot
  subplotx <- pcheck.table(subplot, tab_dsn=data_dsn, tabnm="subplot",
			caption="Subplot table table?")

  ## Check cond
  subp_condx <- pcheck.table(subp_cond, tab_dsn=data_dsn, tabnm="subp_cond",
			caption="Subplot condition table?")



  ## Remove nonsampled subplots
  subpwhereqry <- "subp.SUBP_STATUS_CD <> 3"
  if (ACI) {
    subpwhereqry <- paste(whereqry, 
                " and (c.NF_SUBP_STATUS_CD is NULL or NF_SUBP_STATUS_CD != 3)")
  }

  ## Remove nonsampled conditions
  cwhereqry <- "c.COND_STATUS_CD <> 5"
  if (ACI) {
    cwhereqry <- paste(whereqry, 
                " and (c.NF_COND_STATUS_CD is NULL or NF_COND_STATUS_CD != 5)")
  }

  ## Sum condition proportions by subplot
  #########################################################
  if (!is.null(subplotx)) {

    ## Generate query for calculating subplot-level adjustments
    sumcprop.qry <- paste0(
		 "SELECT c.PLT_CN, c.CONDID, c.COND_STATUS_CD, subc.SUBP, 
			 SUM(COALESCE(subc.SUBPCOND_PROP, 0)) AS SUBPPROP_UNADJ,
			 SUM(COALESCE(subc.MICRCOND_PROP, 0)) AS MICRPROP_UNADJ,
			 SUM(COALESCE(subc.MACRCOND_PROP, 0)) AS MACRPROP_UNADJ,
			 SUM(CASE WHEN subc.MACRCOND_PROP IS NULL 
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
			 SUM(COALESCE(subc.SUBPCOND_PROP, 0)) AS SUBPPROP_UNADJ,
			 SUM(COALESCE(subc.MICRCOND_PROP, 0)) AS MICRPROP_UNADJ,
			 SUM(COALESCE(subc.MACRCOND_PROP, 0)) AS MACRPROP_UNADJ,
			 SUM(CASE WHEN subc.MACRCOND_PROP IS NULL 
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


