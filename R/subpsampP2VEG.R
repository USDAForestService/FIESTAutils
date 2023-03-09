#' @rdname internal_desc
#' @export
subpsampP2VEG <- function(plt, cond, subp_cond, subplot, data_dsn = NULL,
                     subpuniqueid = "PLT_CN", subpid = "SUBP", 
                     ACI = FALSE, whereqry = NULL) {

  ## DESCRIPTION: Summarize sampled subplot condition proportions.

  ## Set global variables
  gui <- FALSE

  ## Check plt
  pltx <- pcheck.table(plt, tab_dsn=data_dsn, tabnm="plt", 
			caption="Plot table?", stopifnull = TRUE)

  ## Check cond
  condx <- pcheck.table(cond, tab_dsn=data_dsn, tabnm="cond", 
			caption="Condition table?", stopifnull = TRUE)

  ## Check subplot
  subplotx <- pcheck.table(subplot, tab_dsn=data_dsn, tabnm="subplot",
			caption="Subplot table table?", stopifnull = TRUE)

  ## Check cond
  subp_condx <- pcheck.table(subp_cond, tab_dsn=data_dsn, tabnm="subp_cond",
			caption="Subplot condition table?", stopifnull = TRUE)



  ############################################################
  ## Assemble where clause
  ############################################################

  if (is.null(whereqry)) {
    ## Remove nonsampled subplots
    subpwhereqry <- "subp.SUBP_STATUS_CD <> 3"
  } else {
    subpwhereqry <- paste(whereqry, 
                  " and subp.SUBP_STATUS_CD <> 3")
  }

  ## Remove nonsampled subplots
  subpwhereqry <- "subp.SUBP_STATUS_CD <> 3"
  if (ACI) {
    subpwhereqry <- paste(subpwhereqry, 
                " and (c.NF_SUBP_STATUS_CD is NULL or NF_SUBP_STATUS_CD != 3)")
  }

  ## Remove nonsampled conditions
  cwhereqry <- "c.COND_STATUS_CD <> 5"
  if (ACI) {
    cwhereqry <- paste(cwhereqry, 
                " and (c.NF_COND_STATUS_CD is NULL or NF_COND_STATUS_CD != 5)")
  }

  ## Get sampled P2 Vegetation data
  p2vegwhereqry <- "p.p2veg_sampling_status_cd < 3 AND 
				((p.samp_method_cd = 1 AND subp.p2veg_subp_status_cd = 1)
                          	OR samp_method_cd = 2)"


  ## Sum condition proportions by subplot
  ############################################################

  ## Generate query for calculating subplot-level adjustments
  sumcprop.qry <- paste0(
      "SELECT c.PLT_CN, c.CONDID, c.COND_STATUS_CD, subc.SUBP,
            SUM(COALESCE(subc.SUBPCOND_PROP, 0)) / 4 AS SUBPPROP_UNADJ_P2VEG,
            SUM(COALESCE(subc.MICRCOND_PROP, 0)) / 4 AS MICRPROP_UNADJ_P2VEG,
            SUM(COALESCE(subc.MACRCOND_PROP, 0)) / 4 AS MACRPROP_UNADJ_P2VEG,
            SUM(CASE WHEN subc.MACRCOND_PROP IS NOT NULL
                     THEN subc.MACRCOND_PROP
                     ELSE subc.SUBPCOND_PROP end) / 4 AS CONDPROP_UNADJ_P2VEG
       FROM pltx p
            JOIN condx c ON (c.PLT_CN = p.CN) 
            JOIN subplotx subp ON (subp.PLT_CN = c.PLT_CN)
            JOIN subp_condx subc ON (subc.PLT_CN = c.PLT_CN
                                        and subc.CONDID = c.CONDID
                                        and subc.SUBP = subp.SUBP)
       WHERE ", p2vegwhereqry, 
                 " AND ", subpwhereqry,
                 " AND ", cwhereqry,
       " GROUP BY c.PLT_CN, c.CONDID, subc.SUBP, c.COND_STATUS_CD")

  vcondx <- data.table(sqldf::sqldf(sumcprop.qry))
  return(vcondx)
}


